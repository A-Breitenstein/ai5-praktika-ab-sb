/*
 * Copyright 2004-2013 H2 Group. Multiple-Licensed under the H2 License,
 * Version 1.0, and under the Eclipse Public License, Version 1.0
 * (http://h2database.com/html/license.html).
 * Initial Developer: H2 Group
 */
package org.h2.command.ddl;

import java.util.ArrayList;
import java.util.HashSet;
import org.h2.command.CommandInterface;
import org.h2.constant.ErrorCode;
import org.h2.constraint.Constraint;
import org.h2.constraint.ConstraintCheck;
import org.h2.constraint.ConstraintReferential;
import org.h2.constraint.ConstraintUnique;
import org.h2.engine.Constants;
import org.h2.engine.Database;
import org.h2.engine.Right;
import org.h2.engine.Session;
import org.h2.expression.Expression;
import org.h2.index.Index;
import org.h2.index.IndexType;
import org.h2.message.DbException;
import org.h2.schema.Schema;
import org.h2.table.Column;
import org.h2.table.IndexColumn;
import org.h2.table.Table;
import org.h2.table.TableFilter;
import org.h2.util.New;

/**
 * This class represents the statement
 * ALTER TABLE ADD CONSTRAINT
 */
public class AlterTableAddConstraint extends SchemaCommand {

    private int type;
    private String constraintName;
    private String tableName;
    private IndexColumn[] indexColumns;
    private int deleteAction;
    private int updateAction;
    private Schema refSchema;
    private String refTableName;
    private IndexColumn[] refIndexColumns;
    private Expression checkExpression;
    private Index index, refIndex;
    private String comment;
    private boolean checkExisting;
    private boolean primaryKeyHash;
    private final boolean ifNotExists;
    private ArrayList<Index> createdIndexes = New.arrayList();

    public AlterTableAddConstraint(Session session, Schema schema, boolean ifNotExists) {
        super(session, schema);
        this.ifNotExists = ifNotExists;
    }

    private String generateConstraintName(Table table) {
        if (constraintName == null) {
            constraintName = getSchema().getUniqueConstraintName(session, table);
        }
        return constraintName;
    }

    @Override
    public int update() {
        try {
            return tryUpdate();
        } catch (DbException e) {
            for (Index index : createdIndexes) {
                session.getDatabase().removeSchemaObject(session, index);
            }
            throw e;
        } finally {
            getSchema().freeUniqueName(constraintName);
        }
    }

    /**
     * Try to execute the statement.
     *
     * @return the update count
     */
    private int tryUpdate() {
        if (!transactional) {
            session.commit(true);
        }
        Database db = session.getDatabase();
        Table table = getSchema().getTableOrView(session, tableName);
        if (getSchema().findConstraint(session, constraintName) != null) {
            if (ifNotExists) {
                return 0;
            }
            throw DbException.get(ErrorCode.CONSTRAINT_ALREADY_EXISTS_1, constraintName);
        }
        session.getUser().checkRight(table, Right.ALL);
        db.lockMeta(session);
        table.lock(session, true, true);
        Constraint constraint;
        switch (type) {
        case CommandInterface.ALTER_TABLE_ADD_CONSTRAINT_PRIMARY_KEY: {
            IndexColumn.mapColumns(indexColumns, table);
            index = table.findPrimaryKey();
            ArrayList<Constraint> constraints = table.getConstraints();
            for (int i = 0; constraints != null && i < constraints.size(); i++) {
                Constraint c = constraints.get(i);
                if (Constraint.PRIMARY_KEY.equals(c.getConstraintType())) {
                    throw DbException.get(ErrorCode.SECOND_PRIMARY_KEY);
                }
            }
            if (index != null) {
                // if there is an index, it must match with the one declared
                // we don't test ascending / descending
                IndexColumn[] pkCols = index.getIndexColumns();
                if (pkCols.length != indexColumns.length) {
                    throw DbException.get(ErrorCode.SECOND_PRIMARY_KEY);
                }
                for (int i = 0; i < pkCols.length; i++) {
                    if (pkCols[i].column != indexColumns[i].column) {
                        throw DbException.get(ErrorCode.SECOND_PRIMARY_KEY);
                    }
                }
            }
            if (index == null) {
                IndexType indexType = IndexType.createPrimaryKey(table.isPersistIndexes(), primaryKeyHash);
                String indexName = table.getSchema().getUniqueIndexName(session, table, Constants.PREFIX_PRIMARY_KEY);
                int id = getObjectId();
                try {
                    index = table.addIndex(session, indexName, id, indexColumns, indexType, true, null);
                } finally {
                    getSchema().freeUniqueName(indexName);
                }
            }
            index.getIndexType().setBelongsToConstraint(true);
            int constraintId = getObjectId();
            String name = generateConstraintName(table);
            ConstraintUnique pk = new ConstraintUnique(getSchema(), constraintId, name, table, true);
            pk.setColumns(indexColumns);
            pk.setIndex(index, true);
            constraint = pk;
            break;
        }
        case CommandInterface.ALTER_TABLE_ADD_CONSTRAINT_UNIQUE: {
            IndexColumn.mapColumns(indexColumns, table);
            boolean isOwner = false;
            if (index != null && canUseUniqueIndex(index, table, indexColumns)) {
                isOwner = true;
                index.getIndexType().setBelongsToConstraint(true);
            } else {
                index = getUniqueIndex(table, indexColumns);
                if (index == null) {
                    index = createIndex(table, indexColumns, true);
                    isOwner = true;
                }
            }
            int id = getObjectId();
            String name = generateConstraintName(table);
            ConstraintUnique unique = new ConstraintUnique(getSchema(), id, name, table, false);
            unique.setColumns(indexColumns);
            unique.setIndex(index, isOwner);
            constraint = unique;
            break;
        }
        case CommandInterface.ALTER_TABLE_ADD_CONSTRAINT_CHECK: {
            int id = getObjectId();
            String name = generateConstraintName(table);
            ConstraintCheck check = new ConstraintCheck(getSchema(), id, name, table);
            TableFilter filter = new TableFilter(session, table, null, false, null);
            checkExpression.mapColumns(filter, 0);
            checkExpression = checkExpression.optimize(session);
            check.setExpression(checkExpression);
            check.setTableFilter(filter);
            constraint = check;
            if (checkExisting) {
                check.checkExistingData(session);
            }
            break;
        }
        case CommandInterface.ALTER_TABLE_ADD_CONSTRAINT_REFERENTIAL: {
            Table refTable = refSchema.getTableOrView(session, refTableName);
            session.getUser().checkRight(refTable, Right.ALL);
            if (!refTable.canReference()) {
                throw DbException.get(ErrorCode.FEATURE_NOT_SUPPORTED_1, "Reference " + refTable.getSQL());
            }
            boolean isOwner = false;
            IndexColumn.mapColumns(indexColumns, table);
            if (index != null && canUseIndex(index, table, indexColumns)) {
                isOwner = true;
                index.getIndexType().setBelongsToConstraint(true);
            } else {
                index = getIndex(table, indexColumns);
                if (index == null) {
                    index = createIndex(table, indexColumns, false);
                    isOwner = true;
                }
            }
            if (refIndexColumns == null) {
                Index refIdx = refTable.getPrimaryKey();
                refIndexColumns = refIdx.getIndexColumns();
            } else {
                IndexColumn.mapColumns(refIndexColumns, refTable);
            }
            if (refIndexColumns.length != indexColumns.length) {
                throw DbException.get(ErrorCode.COLUMN_COUNT_DOES_NOT_MATCH);
            }
            boolean isRefOwner = false;
            if (refIndex != null && refIndex.getTable() == refTable &&
                    canUseIndex(refIndex, refTable, refIndexColumns)) {
                isRefOwner = true;
                refIndex.getIndexType().setBelongsToConstraint(true);
            } else {
                refIndex = null;
            }
            if (refIndex == null) {
                refIndex = getIndex(refTable, refIndexColumns);
                if (refIndex == null) {
                    refIndex = createIndex(refTable, refIndexColumns, true);
                    isRefOwner = true;
                }
            }
            int id = getObjectId();
            String name = generateConstraintName(table);
            ConstraintReferential ref = new ConstraintReferential(getSchema(), id, name, table);
            ref.setColumns(indexColumns);
            ref.setIndex(index, isOwner);
            ref.setRefTable(refTable);
            ref.setRefColumns(refIndexColumns);
            ref.setRefIndex(refIndex, isRefOwner);
            if (checkExisting) {
                ref.checkExistingData(session);
            }
            constraint = ref;
            refTable.addConstraint(constraint);
            ref.setDeleteAction(deleteAction);
            ref.setUpdateAction(updateAction);
            break;
        }
        default:
            throw DbException.throwInternalError("type=" + type);
        }
        // parent relationship is already set with addConstraint
        constraint.setComment(comment);
        if (table.isTemporary() && !table.isGlobalTemporary()) {
            session.addLocalTempTableConstraint(constraint);
        } else {
            db.addSchemaObject(session, constraint);
        }
        table.addConstraint(constraint);
        return 0;
    }

    private Index createIndex(Table t, IndexColumn[] cols, boolean unique) {
        int indexId = getObjectId();
        IndexType indexType;
        if (unique) {
            // for unique constraints
            indexType = IndexType.createUnique(t.isPersistIndexes(), false);
        } else {
            // constraints
            indexType = IndexType.createNonUnique(t.isPersistIndexes());
        }
        indexType.setBelongsToConstraint(true);
        String prefix = constraintName == null ? "CONSTRAINT" : constraintName;
        String indexName = t.getSchema().getUniqueIndexName(session, t, prefix + "_INDEX_");
        try {
            Index index = t.addIndex(session, indexName, indexId, cols, indexType, true, null);
            createdIndexes.add(index);
            return index;
        } finally {
            getSchema().freeUniqueName(indexName);
        }
    }

    public void setDeleteAction(int action) {
        this.deleteAction = action;
    }

    public void setUpdateAction(int action) {
        this.updateAction = action;
    }

    private static Index getUniqueIndex(Table t, IndexColumn[] cols) {
        for (Index idx : t.getIndexes()) {
            if (canUseUniqueIndex(idx, t, cols)) {
                return idx;
            }
        }
        return null;
    }

    private static Index getIndex(Table t, IndexColumn[] cols) {
        for (Index idx : t.getIndexes()) {
            if (canUseIndex(idx, t, cols)) {
                return idx;
            }
        }
        return null;
    }

    private static boolean canUseUniqueIndex(Index idx, Table table, IndexColumn[] cols) {
        if (idx.getTable() != table || !idx.getIndexType().isUnique()) {
            return false;
        }
        Column[] indexCols = idx.getColumns();
        if (indexCols.length > cols.length) {
            return false;
        }
        HashSet<Column> set = New.hashSet();
        for (IndexColumn c : cols) {
            set.add(c.column);
        }
        for (Column c : indexCols) {
            // all columns of the index must be part of the list,
            // but not all columns of the list need to be part of the index
            if (!set.contains(c)) {
                return false;
            }
        }
        return true;
    }

    private static boolean canUseIndex(Index existingIndex, Table table, IndexColumn[] cols) {
        if (existingIndex.getTable() != table || existingIndex.getCreateSQL() == null) {
            // can't use the scan index or index of another table
            return false;
        }
        Column[] indexCols = existingIndex.getColumns();
        if (indexCols.length < cols.length) {
            return false;
        }
        for (IndexColumn col : cols) {
            // all columns of the list must be part of the index,
            // but not all columns of the index need to be part of the list
            // holes are not allowed (index=a,b,c & list=a,b is ok; but list=a,c
            // is not)
            int idx = existingIndex.getColumnIndex(col.column);
            if (idx < 0 || idx >= cols.length) {
                return false;
            }
        }
        return true;
    }

    public void setConstraintName(String constraintName) {
        this.constraintName = constraintName;
    }

    public void setType(int type) {
        this.type = type;
    }

    @Override
    public int getType() {
        return type;
    }

    public void setCheckExpression(Expression expression) {
        this.checkExpression = expression;
    }

    public void setTableName(String tableName) {
        this.tableName = tableName;
    }

    public void setIndexColumns(IndexColumn[] indexColumns) {
        this.indexColumns = indexColumns;
    }

    public IndexColumn[] getIndexColumns() {
        return indexColumns;
    }

    /**
     * Set the referenced table.
     *
     * @param refSchema the schema
     * @param ref the table name
     */
    public void setRefTableName(Schema refSchema, String ref) {
        this.refSchema = refSchema;
        this.refTableName = ref;
    }

    public void setRefIndexColumns(IndexColumn[] indexColumns) {
        this.refIndexColumns = indexColumns;
    }

    public void setIndex(Index index) {
        this.index = index;
    }

    public void setRefIndex(Index refIndex) {
        this.refIndex = refIndex;
    }

    public void setComment(String comment) {
        this.comment = comment;
    }

    public void setCheckExisting(boolean b) {
        this.checkExisting = b;
    }

    public void setPrimaryKeyHash(boolean b) {
        this.primaryKeyHash = b;
    }

}

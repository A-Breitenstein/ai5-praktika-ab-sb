/*
 * Copyright 2004-2013 H2 Group. Multiple-Licensed under the H2 License,
 * Version 1.0, and under the Eclipse Public License, Version 1.0
 * (http://h2database.com/html/license.html).
 * Initial Developer: H2 Group
 */
package org.h2.server;

import java.io.ByteArrayInputStream;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.Socket;
import java.sql.SQLException;
import java.util.ArrayList;
import org.h2.command.Command;
import org.h2.constant.ErrorCode;
import org.h2.constant.SysProperties;
import org.h2.engine.ConnectionInfo;
import org.h2.engine.Constants;
import org.h2.engine.Engine;
import org.h2.engine.Session;
import org.h2.engine.SessionRemote;
import org.h2.expression.Parameter;
import org.h2.expression.ParameterInterface;
import org.h2.expression.ParameterRemote;
import org.h2.jdbc.JdbcSQLException;
import org.h2.message.DbException;
import org.h2.result.ResultColumn;
import org.h2.result.ResultInterface;
import org.h2.store.LobStorageInterface;
import org.h2.util.IOUtils;
import org.h2.util.SmallLRUCache;
import org.h2.util.SmallMap;
import org.h2.util.StringUtils;
import org.h2.value.Transfer;
import org.h2.value.Value;
import org.h2.value.ValueLobDb;

/**
 * One server thread is opened per client connection.
 */
public class TcpServerThread implements Runnable {

    protected final Transfer transfer;
    private final TcpServer server;
    private Session session;
    private boolean stop;
    private Thread thread;
    private Command commit;
    private final SmallMap cache = new SmallMap(SysProperties.SERVER_CACHED_OBJECTS);
    private final SmallLRUCache<Long, CachedInputStream> lobs =
        SmallLRUCache.newInstance(Math.max(
                SysProperties.SERVER_CACHED_OBJECTS,
                SysProperties.SERVER_RESULT_SET_FETCH_SIZE * 5));
    private final int threadId;
    private int clientVersion;
    private String sessionId;

    TcpServerThread(Socket socket, TcpServer server, int id) {
        this.server = server;
        this.threadId = id;
        transfer = new Transfer(null);
        transfer.setSocket(socket);
    }

    private void trace(String s) {
        server.trace(this + " " + s);
    }

    @Override
    public void run() {
        try {
            transfer.init();
            trace("Connect");
            // TODO server: should support a list of allowed databases
            // and a list of allowed clients
            try {
                if (!server.allow(transfer.getSocket())) {
                    throw DbException.get(ErrorCode.REMOTE_CONNECTION_NOT_ALLOWED);
                }
                int minClientVersion = transfer.readInt();
                if (minClientVersion < Constants.TCP_PROTOCOL_VERSION_6) {
                    throw DbException.get(ErrorCode.DRIVER_VERSION_ERROR_2, "" + clientVersion, "" + Constants.TCP_PROTOCOL_VERSION_6);
                } else if (minClientVersion > Constants.TCP_PROTOCOL_VERSION_12) {
                    throw DbException.get(ErrorCode.DRIVER_VERSION_ERROR_2, "" + clientVersion, "" + Constants.TCP_PROTOCOL_VERSION_12);
                }
                int maxClientVersion = transfer.readInt();
                if (maxClientVersion >= Constants.TCP_PROTOCOL_VERSION_12) {
                    clientVersion = Constants.TCP_PROTOCOL_VERSION_12;
                } else {
                    clientVersion = minClientVersion;
                }
                transfer.setVersion(clientVersion);
                String db = transfer.readString();
                String originalURL = transfer.readString();
                if (db == null && originalURL == null) {
                    String targetSessionId = transfer.readString();
                    int command = transfer.readInt();
                    stop = true;
                    if (command == SessionRemote.SESSION_CANCEL_STATEMENT) {
                        // cancel a running statement
                        int statementId = transfer.readInt();
                        server.cancelStatement(targetSessionId, statementId);
                    } else if (command == SessionRemote.SESSION_CHECK_KEY) {
                        // check if this is the correct server
                        db = server.checkKeyAndGetDatabaseName(targetSessionId);
                        if (!targetSessionId.equals(db)) {
                            transfer.writeInt(SessionRemote.STATUS_OK);
                        } else {
                            transfer.writeInt(SessionRemote.STATUS_ERROR);
                        }
                    }
                }
                String baseDir = server.getBaseDir();
                if (baseDir == null) {
                    baseDir = SysProperties.getBaseDir();
                }
                db = server.checkKeyAndGetDatabaseName(db);
                ConnectionInfo ci = new ConnectionInfo(db);
                ci.setOriginalURL(originalURL);
                ci.setUserName(transfer.readString());
                ci.setUserPasswordHash(transfer.readBytes());
                ci.setFilePasswordHash(transfer.readBytes());
                int len = transfer.readInt();
                for (int i = 0; i < len; i++) {
                    ci.setProperty(transfer.readString(), transfer.readString());
                }
                // override client's requested properties with server settings
                if (baseDir != null) {
                    ci.setBaseDir(baseDir);
                }
                if (server.getIfExists()) {
                    ci.setProperty("IFEXISTS", "TRUE");
                }
                session = Engine.getInstance().createSession(ci);
                transfer.setSession(session);
                transfer.writeInt(SessionRemote.STATUS_OK);
                transfer.writeInt(clientVersion);
                transfer.flush();
                server.addConnection(threadId, originalURL, ci.getUserName());
                trace("Connected");
            } catch (Throwable e) {
                sendError(e);
                stop = true;
            }
            while (!stop) {
                try {
                    process();
                } catch (Throwable e) {
                    sendError(e);
                }
            }
            trace("Disconnect");
        } catch (Throwable e) {
            server.traceError(e);
        } finally {
            close();
        }
    }

    private void closeSession() {
        if (session != null) {
            RuntimeException closeError = null;
            try {
                Command rollback = session.prepareLocal("ROLLBACK");
                rollback.executeUpdate();
            } catch (RuntimeException e) {
                closeError = e;
                server.traceError(e);
            } catch (Exception e) {
                server.traceError(e);
            }
            try {
                session.close();
                server.removeConnection(threadId);
            } catch (RuntimeException e) {
                if (closeError == null) {
                    closeError = e;
                    server.traceError(e);
                }
            } catch (Exception e) {
                server.traceError(e);
            } finally {
                session = null;
            }
            if (closeError != null) {
                throw closeError;
            }
        }
    }

    /**
     * Close a connection.
     */
    void close() {
        try {
            stop = true;
            closeSession();
        } catch (Exception e) {
            server.traceError(e);
        } finally {
            transfer.close();
            trace("Close");
            server.remove(this);
        }
    }

    private void sendError(Throwable t) {
        try {
            SQLException e = DbException.convert(t).getSQLException();
            StringWriter writer = new StringWriter();
            e.printStackTrace(new PrintWriter(writer));
            String trace = writer.toString();
            String message;
            String sql;
            if (e instanceof JdbcSQLException) {
                JdbcSQLException j = (JdbcSQLException) e;
                message = j.getOriginalMessage();
                sql = j.getSQL();
            } else {
                message = e.getMessage();
                sql = null;
            }
            transfer.writeInt(SessionRemote.STATUS_ERROR).writeString(e.getSQLState()).writeString(message)
                    .writeString(sql).writeInt(e.getErrorCode()).writeString(trace).flush();
        } catch (Exception e2) {
            if (!transfer.isClosed()) {
                server.traceError(e2);
            }
            // if writing the error does not work, close the connection
            stop = true;
        }
    }

    private void setParameters(Command command) throws IOException {
        int len = transfer.readInt();
        ArrayList<? extends ParameterInterface> params = command.getParameters();
        for (int i = 0; i < len; i++) {
            Parameter p = (Parameter) params.get(i);
            p.setValue(transfer.readValue());
        }
    }

    private void process() throws IOException {
        int operation = transfer.readInt();
        switch (operation) {
        case SessionRemote.SESSION_PREPARE_READ_PARAMS:
        case SessionRemote.SESSION_PREPARE: {
            int id = transfer.readInt();
            String sql = transfer.readString();
            int old = session.getModificationId();
            Command command = session.prepareLocal(sql);
            boolean readonly = command.isReadOnly();
            cache.addObject(id, command);
            boolean isQuery = command.isQuery();
            ArrayList<? extends ParameterInterface> params = command.getParameters();
            transfer.writeInt(getState(old)).writeBoolean(isQuery).writeBoolean(readonly)
                    .writeInt(params.size());
            if (operation == SessionRemote.SESSION_PREPARE_READ_PARAMS) {
                for (ParameterInterface p : params) {
                    ParameterRemote.writeMetaData(transfer, p);
                }
            }
            transfer.flush();
            break;
        }
        case SessionRemote.SESSION_CLOSE: {
            stop = true;
            closeSession();
            transfer.writeInt(SessionRemote.STATUS_OK).flush();
            close();
            break;
        }
        case SessionRemote.COMMAND_COMMIT: {
            if (commit == null) {
                commit = session.prepareLocal("COMMIT");
            }
            int old = session.getModificationId();
            commit.executeUpdate();
            transfer.writeInt(getState(old)).flush();
            break;
        }
        case SessionRemote.COMMAND_GET_META_DATA: {
            int id = transfer.readInt();
            int objectId = transfer.readInt();
            Command command = (Command) cache.getObject(id, false);
            ResultInterface result = command.getMetaData();
            cache.addObject(objectId, result);
            int columnCount = result.getVisibleColumnCount();
            transfer.writeInt(SessionRemote.STATUS_OK).writeInt(columnCount).writeInt(0);
            for (int i = 0; i < columnCount; i++) {
                ResultColumn.writeColumn(transfer, result, i);
            }
            transfer.flush();
            break;
        }
        case SessionRemote.COMMAND_EXECUTE_QUERY: {
            int id = transfer.readInt();
            int objectId = transfer.readInt();
            int maxRows = transfer.readInt();
            int fetchSize = transfer.readInt();
            Command command = (Command) cache.getObject(id, false);
            setParameters(command);
            int old = session.getModificationId();
            ResultInterface result;
            synchronized (session) {
                result = command.executeQuery(maxRows, false);
            }
            cache.addObject(objectId, result);
            int columnCount = result.getVisibleColumnCount();
            int state = getState(old);
            transfer.writeInt(state).writeInt(columnCount);
            int rowCount = result.getRowCount();
            transfer.writeInt(rowCount);
            for (int i = 0; i < columnCount; i++) {
                ResultColumn.writeColumn(transfer, result, i);
            }
            int fetch = Math.min(rowCount, fetchSize);
            for (int i = 0; i < fetch; i++) {
                sendRow(result);
            }
            transfer.flush();
            break;
        }
        case SessionRemote.COMMAND_EXECUTE_UPDATE: {
            int id = transfer.readInt();
            Command command = (Command) cache.getObject(id, false);
            setParameters(command);
            int old = session.getModificationId();
            int updateCount;
            synchronized (session) {
                updateCount = command.executeUpdate();
            }
            int status;
            if (session.isClosed()) {
                status = SessionRemote.STATUS_CLOSED;
            } else {
                status = getState(old);
            }
            transfer.writeInt(status).writeInt(updateCount).writeBoolean(session.getAutoCommit());
            transfer.flush();
            break;
        }
        case SessionRemote.COMMAND_CLOSE: {
            int id = transfer.readInt();
            Command command = (Command) cache.getObject(id, true);
            if (command != null) {
                command.close();
                cache.freeObject(id);
            }
            break;
        }
        case SessionRemote.RESULT_FETCH_ROWS: {
            int id = transfer.readInt();
            int count = transfer.readInt();
            ResultInterface result = (ResultInterface) cache.getObject(id, false);
            transfer.writeInt(SessionRemote.STATUS_OK);
            for (int i = 0; i < count; i++) {
                sendRow(result);
            }
            transfer.flush();
            break;
        }
        case SessionRemote.RESULT_RESET: {
            int id = transfer.readInt();
            ResultInterface result = (ResultInterface) cache.getObject(id, false);
            result.reset();
            break;
        }
        case SessionRemote.RESULT_CLOSE: {
            int id = transfer.readInt();
            ResultInterface result = (ResultInterface) cache.getObject(id, true);
            if (result != null) {
                result.close();
                cache.freeObject(id);
            }
            break;
        }
        case SessionRemote.CHANGE_ID: {
            int oldId = transfer.readInt();
            int newId = transfer.readInt();
            Object obj = cache.getObject(oldId, false);
            cache.freeObject(oldId);
            cache.addObject(newId, obj);
            break;
        }
        case SessionRemote.SESSION_SET_ID: {
            sessionId = transfer.readString();
            transfer.writeInt(SessionRemote.STATUS_OK).flush();
            break;
        }
        case SessionRemote.SESSION_SET_AUTOCOMMIT: {
            boolean autoCommit = transfer.readBoolean();
            session.setAutoCommit(autoCommit);
            transfer.writeInt(SessionRemote.STATUS_OK).flush();
            break;
        }
        case SessionRemote.SESSION_HAS_PENDING_TRANSACTION: {
            transfer.writeInt(SessionRemote.STATUS_OK).
                writeInt(session.hasPendingTransaction() ? 1 : 0).flush();
            break;
        }
        case SessionRemote.LOB_READ: {
            long lobId = transfer.readLong();
            byte[] hmac;
            CachedInputStream in;
            if (clientVersion >= Constants.TCP_PROTOCOL_VERSION_11) {
                if (clientVersion >= Constants.TCP_PROTOCOL_VERSION_12) {
                    hmac = transfer.readBytes();
                    transfer.verifyLobMac(hmac, lobId);
                } else {
                    hmac = null;
                }
                in = lobs.get(lobId);
                if (in == null) {
                    in = new CachedInputStream(null);
                    lobs.put(lobId, in);
                }
            } else {
                hmac = null;
                in = lobs.get(lobId);
                if (in == null) {
                    throw DbException.get(ErrorCode.OBJECT_CLOSED);
                }
            }
            long offset = transfer.readLong();
            if (in.getPos() != offset) {
                LobStorageInterface lobStorage = session.getDataHandler().getLobStorage();
                InputStream lobIn = lobStorage.getInputStream(lobId, hmac, -1);
                in = new CachedInputStream(lobIn);
                lobs.put(lobId, in);
                lobIn.skip(offset);
            }
            int length = transfer.readInt();
            // limit the buffer size
            length = Math.min(16 * Constants.IO_BUFFER_SIZE, length);
            transfer.writeInt(SessionRemote.STATUS_OK);
            byte[] buff = new byte[length];
            length = IOUtils.readFully(in, buff, 0, length);
            transfer.writeInt(length);
            transfer.writeBytes(buff, 0, length);
            transfer.flush();
            break;
        }
        default:
            trace("Unknown operation: " + operation);
            closeSession();
            close();
        }
    }

    private int getState(int oldModificationId) {
        if (session.getModificationId() == oldModificationId) {
            return SessionRemote.STATUS_OK;
        }
        return SessionRemote.STATUS_OK_STATE_CHANGED;
    }

    private void sendRow(ResultInterface result) throws IOException {
        if (result.next()) {
            transfer.writeBoolean(true);
            Value[] v = result.currentRow();
            for (int i = 0; i < result.getVisibleColumnCount(); i++) {
                if (clientVersion >= Constants.TCP_PROTOCOL_VERSION_12) {
                    transfer.writeValue(v[i]);
                } else {
                    writeValue(v[i]);
                }
            }
        } else {
            transfer.writeBoolean(false);
        }
    }

    private void writeValue(Value v) throws IOException {
        if (v.getType() == Value.CLOB || v.getType() == Value.BLOB) {
            if (v instanceof ValueLobDb) {
                ValueLobDb lob = (ValueLobDb) v;
                if (lob.isStored()) {
                    long id = lob.getLobId();
                    lobs.put(id, new CachedInputStream(null));
                }
            }
        }
        transfer.writeValue(v);
    }

    void setThread(Thread thread) {
        this.thread = thread;
    }

    Thread getThread() {
        return thread;
    }

    /**
     * Cancel a running statement.
     *
     * @param targetSessionId the session id
     * @param statementId the statement to cancel
     */
    void cancelStatement(String targetSessionId, int statementId) {
        if (StringUtils.equals(targetSessionId, this.sessionId)) {
            Command cmd = (Command) cache.getObject(statementId, false);
            cmd.cancel();
        }
    }

    /**
     * An input stream with a position.
     */
    static class CachedInputStream extends FilterInputStream {

        private static final ByteArrayInputStream DUMMY = new ByteArrayInputStream(new byte[0]);
        private long pos;

        CachedInputStream(InputStream in) {
            super(in == null ? DUMMY : in);
            if (in == null) {
                pos = -1;
            }
        }

        @Override
        public int read(byte[] buff, int off, int len) throws IOException {
            len = super.read(buff, off, len);
            if (len > 0) {
                pos += len;
            }
            return len;
        }

        @Override
        public int read() throws IOException {
            int x = in.read();
            if (x >= 0) {
                pos++;
            }
            return x;
        }

        @Override
        public long skip(long n) throws IOException {
            n = super.skip(n);
            if (n > 0) {
                pos += n;
            }
            return n;
        }

        public long getPos() {
            return pos;
        }

    }

}

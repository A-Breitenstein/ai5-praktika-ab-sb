/*
 * Copyright 2004-2013 H2 Group. Multiple-Licensed under the H2 License,
 * Version 1.0, and under the Eclipse Public License, Version 1.0
 * (http://h2database.com/html/license.html).
 * Initial Developer: H2 Group
 */
package org.h2.test.db;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;

import org.h2.constant.ErrorCode;
import org.h2.store.fs.FileUtils;
import org.h2.test.TestBase;

/**
 * Access rights tests.
 */
public class TestRights extends TestBase {

    private Statement stat;

    /**
     * Run just this test.
     *
     * @param a ignored
     */
    public static void main(String... a) throws Exception {
        TestBase.createCaller().init().test();
    }

    @Override
    public void test() throws SQLException {
        testOpenNonAdminWithMode();
        testDisallowedTables();
        testDropOwnUser();
        testGetTables();
        testDropTempTables();
        // testLowerCaseUser();
        testSchemaRenameUser();
        testAccessRights();
        deleteDb("rights");
    }

    private void testOpenNonAdminWithMode() throws SQLException {
        if (config.memory) {
            return;
        }
        deleteDb("rights");
        Connection conn = getConnection("rights;MODE=MYSQL");
        stat = conn.createStatement();
        stat.execute("create user test password 'test'");
        Connection conn2 = getConnection("rights;MODE=MYSQL", "test", getPassword("test"));
        conn2.close();
        conn.close();
        if (config.memory) {
            return;
        }
        // if opening alone
        conn2 = getConnection("rights;MODE=MYSQL", "test", getPassword("test"));
        conn2.close();
        // if opening as the second connection
        conn = getConnection("rights;MODE=MYSQL");
        conn2 = getConnection("rights;MODE=MYSQL", "test", getPassword("test"));
        conn2.close();
        stat = conn.createStatement();
        stat.execute("drop user test");
        conn.close();
    }

    private void testDisallowedTables() throws SQLException {
        deleteDb("rights");
        Connection conn = getConnection("rights");
        stat = conn.createStatement();

        stat.execute("CREATE USER IF NOT EXISTS TEST PASSWORD 'TEST'");
        stat.execute("CREATE ROLE TEST_ROLE");
        stat.execute("CREATE TABLE ADMIN_ONLY(ID INT)");
        stat.execute("CREATE TABLE TEST(ID INT)");
        stat.execute("GRANT ALL ON TEST TO TEST");
        Connection conn2 = getConnection("rights", "TEST", getPassword("TEST"));
        Statement stat2 = conn2.createStatement();

        String sql = "select * from admin_only where 1=0";
        stat.execute(sql);
        assertThrows(ErrorCode.NOT_ENOUGH_RIGHTS_FOR_1, stat2).execute(sql);

        DatabaseMetaData meta = conn2.getMetaData();
        ResultSet rs;
        rs = meta.getTables(null, null, "%", new String[]{"TABLE", "VIEW", "SEQUENCE"});
        assertTrue(rs.next());
        assertTrue(rs.next());
        assertFalse(rs.next());
        for (String s : new String[] {
                "information_schema.settings where name='property.java.runtime.version'",
                "information_schema.users where name='SA'",
                "information_schema.roles",
                "information_schema.rights",
                "information_schema.sessions where user_name='SA'"
                }) {
            rs = stat2.executeQuery("select * from " + s);
            assertFalse(rs.next());
            rs = stat.executeQuery("select * from " + s);
            assertTrue(rs.next());
        }
        conn2.close();
        conn.close();
    }
    private void testDropOwnUser() throws SQLException {
        deleteDb("rights");
        String user = getUser().toUpperCase();
        Connection conn = getConnection("rights");
        stat = conn.createStatement();
        assertThrows(ErrorCode.CANNOT_DROP_CURRENT_USER, stat).
                execute("DROP USER " + user);
        stat.execute("CREATE USER TEST PASSWORD 'TEST' ADMIN");
        stat.execute("DROP USER " + user);
        conn.close();
        if (!config.memory) {
            assertThrows(ErrorCode.WRONG_USER_OR_PASSWORD, this).
                    getConnection("rights");
        }
    }

//    public void testLowerCaseUser() throws SQLException {
    // Documentation: for compatibility,
    // only unquoted or uppercase user names are allowed.
//        deleteDb("rights");
//        Connection conn = getConnection("rights");
//        stat = conn.createStatement();
//        stat.execute("CREATE USER \"TEST1\" PASSWORD 'abc'");
//        stat.execute("CREATE USER \"Test2\" PASSWORD 'abc'");
//        conn.close();
//        conn = getConnection("rights", "TEST1", "abc");
//        conn.close();
//        conn = getConnection("rights", "Test2", "abc");
//        conn.close();
//    }

    private void testGetTables() throws SQLException {
        deleteDb("rights");
        Connection conn = getConnection("rights");
        stat = conn.createStatement();

        stat.execute("CREATE USER IF NOT EXISTS TEST PASSWORD 'TEST'");
        stat.execute("CREATE TABLE TEST(ID INT)");
        stat.execute("GRANT ALL ON TEST TO TEST");
        Connection conn2 = getConnection("rights", "TEST", getPassword("TEST"));
        DatabaseMetaData meta = conn2.getMetaData();
        meta.getTables(null, null, "%", new String[]{"TABLE", "VIEW", "SEQUENCE"});
        conn2.close();
        conn.close();
    }

    private void testDropTempTables() throws SQLException {
        deleteDb("rights");
        Connection conn = getConnection("rights");
        stat = conn.createStatement();
        stat.execute("CREATE USER IF NOT EXISTS READER PASSWORD 'READER'");
        stat.execute("CREATE TABLE TEST(ID INT)");
        Connection conn2 = getConnection("rights", "READER", getPassword("READER"));
        Statement stat2 = conn2.createStatement();
        assertThrows(ErrorCode.NOT_ENOUGH_RIGHTS_FOR_1, stat2).
                execute("SELECT * FROM TEST");
        stat2.execute("CREATE LOCAL TEMPORARY TABLE IF NOT EXISTS MY_TEST(ID INT)");
        stat2.execute("INSERT INTO MY_TEST VALUES(1)");
        stat2.execute("SELECT * FROM MY_TEST");
        stat2.execute("DROP TABLE MY_TEST");
        conn2.close();
        conn.close();
    }

    private void testSchemaRenameUser() throws SQLException {
        if (config.memory) {
            return;
        }
        deleteDb("rights");
        Connection conn = getConnection("rights");
        stat = conn.createStatement();
        stat.execute("create user test password '' admin");
        stat.execute("create schema b authorization test");
        stat.execute("create table b.test(id int)");
        stat.execute("alter user test rename to test1");
        conn.close();
        conn = getConnection("rights");
        stat = conn.createStatement();
        stat.execute("select * from b.test");
        assertThrows(ErrorCode.CANNOT_DROP_2, stat).
                execute("alter user test1 admin false");
        assertThrows(ErrorCode.CANNOT_DROP_2, stat).
                execute("drop user test1");
        stat.execute("drop schema b");
        stat.execute("alter user test1 admin false");
        stat.execute("drop user test1");
        conn.close();
    }

    private void testAccessRights() throws SQLException {
        if (config.memory) {
            return;
        }

        deleteDb("rights");
        Connection conn = getConnection("rights");
        stat = conn.createStatement();
        // default table type
        testTableType(conn, "MEMORY");
        testTableType(conn, "CACHED");

        // rights on tables and views
        executeSuccess("CREATE USER PASS_READER PASSWORD 'abc'");
        executeSuccess("CREATE TABLE TEST(ID INT PRIMARY KEY, NAME VARCHAR)");
        executeSuccess("CREATE TABLE PASS(ID INT PRIMARY KEY, NAME VARCHAR, PASSWORD VARCHAR)");
        executeSuccess("CREATE VIEW PASS_NAME AS SELECT ID, NAME FROM PASS");
        executeSuccess("GRANT SELECT ON PASS_NAME TO PASS_READER");
        executeSuccess("GRANT SELECT, INSERT, UPDATE ON TEST TO PASS_READER");
        conn.close();

        conn = getConnection("rights;LOG=2", "PASS_READER", getPassword("abc"));
        stat = conn.createStatement();
        executeSuccess("SELECT * FROM PASS_NAME");
        executeSuccess("SELECT * FROM (SELECT * FROM PASS_NAME)");
        executeSuccess("SELECT (SELECT NAME FROM PASS_NAME) P FROM PASS_NAME");
        executeError("SELECT (SELECT PASSWORD FROM PASS) P FROM PASS_NAME");
        executeError("SELECT * FROM PASS");
        executeError("INSERT INTO TEST SELECT 1, PASSWORD FROM PASS");
        executeError("INSERT INTO TEST VALUES(SELECT PASSWORD FROM PASS)");
        executeError("UPDATE TEST SET NAME=(SELECT PASSWORD FROM PASS)");
        executeError("DELETE FROM TEST WHERE NAME=(SELECT PASSWORD FROM PASS)");
        executeError("SELECT * FROM (SELECT * FROM PASS)");
        assertThrows(ErrorCode.TABLE_OR_VIEW_NOT_FOUND_1, stat).
                execute("CREATE VIEW X AS SELECT * FROM PASS_READER");
        conn.close();

        conn = getConnection("rights");
        stat = conn.createStatement();

        executeSuccess("DROP TABLE TEST");
        executeSuccess("CREATE USER TEST PASSWORD 'abc'");
        executeSuccess("ALTER USER TEST ADMIN TRUE");
        executeSuccess("CREATE TABLE TEST(ID INT)");
        executeSuccess("CREATE SCHEMA SCHEMA_A AUTHORIZATION SA");
        executeSuccess("CREATE TABLE SCHEMA_A.TABLE_B(ID INT)");
        executeSuccess("GRANT ALL ON SCHEMA_A.TABLE_B TO TEST");
        executeSuccess("CREATE TABLE HIDDEN(ID INT)");
        executeSuccess("CREATE TABLE PUB_TABLE(ID INT)");
        executeSuccess("CREATE TABLE ROLE_TABLE(ID INT)");
        executeSuccess("CREATE ROLE TEST_ROLE");
        executeSuccess("GRANT SELECT ON ROLE_TABLE TO TEST_ROLE");
        executeSuccess("GRANT UPDATE ON ROLE_TABLE TO TEST_ROLE");
        executeSuccess("REVOKE UPDATE ON ROLE_TABLE FROM TEST_ROLE");
        assertThrows(ErrorCode.ROLES_AND_RIGHT_CANNOT_BE_MIXED, stat).
                execute("REVOKE SELECT, SUB1 ON ROLE_TABLE FROM TEST_ROLE");
        executeSuccess("GRANT TEST_ROLE TO TEST");
        executeSuccess("GRANT SELECT ON PUB_TABLE TO PUBLIC");
        executeSuccess("GRANT SELECT ON TEST TO TEST");
        executeSuccess("CREATE ROLE SUB1");
        executeSuccess("CREATE ROLE SUB2");
        executeSuccess("CREATE TABLE SUB_TABLE(ID INT)");
        executeSuccess("GRANT ALL ON SUB_TABLE TO SUB2");
        executeSuccess("REVOKE UPDATE, DELETE ON SUB_TABLE FROM SUB2");
        executeSuccess("GRANT SUB2 TO SUB1");
        executeSuccess("GRANT SUB1 TO TEST");

        executeSuccess("ALTER USER TEST SET PASSWORD 'def'");
        executeSuccess("CREATE USER TEST2 PASSWORD 'def' ADMIN");
        executeSuccess("ALTER USER TEST ADMIN FALSE");
        executeSuccess("SCRIPT TO '" + getBaseDir() + "/rights.sql' CIPHER XTEA PASSWORD 'test'");
        conn.close();

        try {
            getConnection("rights", "Test", getPassword("abc"));
            fail("mixed case user name");
        } catch (SQLException e) {
            assertKnownException(e);
        }
        try {
            getConnection("rights", "TEST", getPassword("abc"));
            fail("wrong password");
        } catch (SQLException e) {
            assertKnownException(e);
        }
        try {
            getConnection("rights", "TEST", getPassword(""));
            fail("wrong password");
        } catch (SQLException e) {
            assertKnownException(e);
        }
        conn = getConnection("rights;LOG=2", "TEST", getPassword("def"));
        stat = conn.createStatement();

        assertThrows(ErrorCode.ADMIN_RIGHTS_REQUIRED, stat).
                execute("SET DEFAULT_TABLE_TYPE MEMORY");

        executeSuccess("SELECT * FROM TEST");
        executeSuccess("SELECT * FROM SYSTEM_RANGE(1,2)");
        executeSuccess("SELECT * FROM SCHEMA_A.TABLE_B");
        executeSuccess("SELECT * FROM PUB_TABLE");
        executeSuccess("SELECT * FROM ROLE_TABLE");
        executeError("UPDATE ROLE_TABLE SET ID=0");
        executeError("DELETE FROM ROLE_TABLE");
        executeError("SELECT * FROM HIDDEN");
        executeError("UPDATE TEST SET ID=0");
        executeError("CALL SELECT MIN(PASSWORD) FROM PASS");
        executeSuccess("SELECT * FROM SUB_TABLE");
        executeSuccess("INSERT INTO SUB_TABLE VALUES(1)");
        executeError("DELETE FROM SUB_TABLE");
        executeError("UPDATE SUB_TABLE SET ID=0");

        assertThrows(ErrorCode.ADMIN_RIGHTS_REQUIRED, stat).
                execute("CREATE USER TEST3 PASSWORD 'def'");
        assertThrows(ErrorCode.ADMIN_RIGHTS_REQUIRED, stat).
                execute("ALTER USER TEST2 ADMIN FALSE");
        assertThrows(ErrorCode.ADMIN_RIGHTS_REQUIRED, stat).
                execute("ALTER USER TEST2 SET PASSWORD 'ghi'");
        assertThrows(ErrorCode.ADMIN_RIGHTS_REQUIRED, stat).
                execute("ALTER USER TEST2 RENAME TO TEST_X");
        assertThrows(ErrorCode.ADMIN_RIGHTS_REQUIRED, stat).
                execute("ALTER USER TEST RENAME TO TEST_X");
        executeSuccess("ALTER USER TEST SET PASSWORD 'ghi'");
        assertThrows(ErrorCode.ADMIN_RIGHTS_REQUIRED, stat).
                execute("DROP USER TEST2");

        conn.close();
        conn = getConnection("rights");
        stat = conn.createStatement();
        executeSuccess("DROP ROLE SUB1");
        executeSuccess("DROP TABLE ROLE_TABLE");
        executeSuccess("DROP USER TEST");

        conn.close();
        conn = getConnection("rights");
        stat = conn.createStatement();

        executeSuccess("DROP TABLE IF EXISTS TEST");
        executeSuccess("CREATE TABLE TEST(ID INT PRIMARY KEY, NAME VARCHAR(255))");
        executeSuccess("CREATE USER GUEST PASSWORD 'abc'");
        executeSuccess("GRANT SELECT ON TEST TO GUEST");
        executeSuccess("ALTER USER GUEST RENAME TO GAST");
        conn.close();
        conn = getConnection("rights");
        conn.close();
        FileUtils.delete(getBaseDir() + "/rights.sql");
    }

    private void testTableType(Connection conn, String type) throws SQLException {
        executeSuccess("SET DEFAULT_TABLE_TYPE " + type);
        executeSuccess("CREATE TABLE TEST(ID INT)");
        ResultSet rs = conn.createStatement().executeQuery(
                "SELECT STORAGE_TYPE FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME='TEST'");
        rs.next();
        assertEquals(type, rs.getString(1));
        executeSuccess("DROP TABLE TEST");
    }

    private void executeError(String sql) throws SQLException {
        assertThrows(ErrorCode.NOT_ENOUGH_RIGHTS_FOR_1, stat).execute(sql);
    }

    private void executeSuccess(String sql) throws SQLException {
        if (stat.execute(sql)) {
            ResultSet rs = stat.getResultSet();

            // this will check if the result set is updatable
            rs.getConcurrency();

            ResultSetMetaData meta = rs.getMetaData();
            int columnCount = meta.getColumnCount();
            for (int i = 0; i < columnCount; i++) {
                meta.getCatalogName(i + 1);
                meta.getColumnClassName(i + 1);
                meta.getColumnDisplaySize(i + 1);
                meta.getColumnLabel(i + 1);
                meta.getColumnName(i + 1);
                meta.getColumnType(i + 1);
                meta.getColumnTypeName(i + 1);
                meta.getPrecision(i + 1);
                meta.getScale(i + 1);
                meta.getSchemaName(i + 1);
                meta.getTableName(i + 1);
            }
            while (rs.next()) {
                for (int i = 0; i < columnCount; i++) {
                    rs.getObject(i + 1);
                }
            }
        }
    }

}

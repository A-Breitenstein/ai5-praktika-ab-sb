/*
 * Copyright 2004-2013 H2 Group. Multiple-Licensed under the H2 License,
 * Version 1.0, and under the Eclipse Public License, Version 1.0
 * (http://h2database.com/html/license.html).
 * Initial Developer: H2 Group
 */
package org.h2.test.db;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Random;

import org.h2.result.SortOrder;
import org.h2.test.TestBase;
import org.h2.util.New;

/**
 * Index tests.
 */
public class TestIndex extends TestBase {

    private Connection conn;
    private Statement stat;
    private final Random random = new Random();

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
        deleteDb("index");
        testErrorMessage();
        testNonUniqueHashIndex();
        testRenamePrimaryKey();
        testRandomized();
        testDescIndex();
        testHashIndex();

        if (config.networked && config.big) {
            return;
        }

        random.setSeed(100);

        deleteDb("index");
        testWideIndex(147);
        testWideIndex(313);
        testWideIndex(979);
        testWideIndex(1200);
        testWideIndex(2400);
        if (config.big) {
            Random r = new Random();
            for (int j = 0; j < 10; j++) {
                int i = r.nextInt(3000);
                if ((i % 100) == 0) {
                    println("width: " + i);
                }
                testWideIndex(i);
            }
        }

        testLike();
        reconnect();
        testConstraint();
        testLargeIndex();
        testMultiColumnIndex();
        // long time;
        // time = System.currentTimeMillis();
        testHashIndex(true, false);

        testHashIndex(false, false);
        // System.out.println("b-tree="+(System.currentTimeMillis()-time));
        // time = System.currentTimeMillis();
        testHashIndex(true, true);
        testHashIndex(false, true);
        // System.out.println("hash="+(System.currentTimeMillis()-time));

        testMultiColumnHashIndex();

        conn.close();
        deleteDb("index");
    }

    private void testErrorMessage() throws SQLException {
        reconnect();
        stat.execute("create table test(id int primary key, name varchar)");
        testErrorMessage("PRIMARY", "KEY", " ON PUBLIC.TEST(ID)");
        stat.execute("create table test(id int, name varchar primary key)");
        testErrorMessage("PRIMARY_KEY_2 ON PUBLIC.TEST(NAME)");
        stat.execute("create table test(id int, name varchar, primary key(id, name))");
        testErrorMessage("PRIMARY_KEY_2 ON PUBLIC.TEST(ID, NAME)");
        stat.execute("create table test(id int, name varchar, primary key(name, id))");
        testErrorMessage("PRIMARY_KEY_2 ON PUBLIC.TEST(NAME, ID)");
        stat.execute("create table test(id int, name int primary key)");
        testErrorMessage("PRIMARY", "KEY", " ON PUBLIC.TEST(NAME)");
        stat.execute("create table test(id int, name int, unique(name))");
        testErrorMessage("CONSTRAINT_INDEX_2 ON PUBLIC.TEST(NAME)");
        stat.execute("create table test(id int, name int, constraint abc unique(name, id))");
        testErrorMessage("ABC_INDEX_2 ON PUBLIC.TEST(NAME, ID)");
    }

    private void testErrorMessage(String... expected) throws SQLException {
        try {
            stat.execute("INSERT INTO TEST VALUES(1, 1)");
            stat.execute("INSERT INTO TEST VALUES(1, 1)");
            fail();
        } catch (SQLException e) {
            String m = e.getMessage();
            int start = m.indexOf('\"'), end = m.indexOf('\"', start + 1);
            String s = m.substring(start + 1, end);
            for (String t : expected) {
                assertTrue(t + " not in " + s, s.indexOf(t) >= 0);
            }
        }
        stat.execute("drop table test");
    }

    private void testNonUniqueHashIndex() throws SQLException {
        reconnect();
        stat.execute("create memory table test(id bigint, data bigint)");
        stat.execute("create hash index on test(id)");
        Random rand = new Random(1);
        PreparedStatement prepInsert = conn.prepareStatement("insert into test values(?, ?)");
        PreparedStatement prepDelete = conn.prepareStatement("delete from test where id=?");
        PreparedStatement prepSelect = conn.prepareStatement("select count(*) from test where id=?");
        HashMap<Long, Integer> map = New.hashMap();
        for (int i = 0; i < 1000; i++) {
            long key = rand.nextInt(10) * 1000000000L;
            Integer r = map.get(key);
            int result = r == null ? 0 : (int) r;
            if (rand.nextBoolean()) {
                prepSelect.setLong(1, key);
                ResultSet rs = prepSelect.executeQuery();
                rs.next();
                assertEquals(result, rs.getInt(1));
            } else {
                if (rand.nextBoolean()) {
                    prepInsert.setLong(1, key);
                    prepInsert.setInt(2, rand.nextInt());
                    prepInsert.execute();
                    map.put(key, result + 1);
                } else {
                    prepDelete.setLong(1, key);
                    prepDelete.execute();
                    map.put(key, 0);
                }
            }
        }
        stat.execute("drop table test");
        conn.close();
    }

    private void testRenamePrimaryKey() throws SQLException {
        if (config.memory) {
            return;
        }
        reconnect();
        stat.execute("create table test(id int not null)");
        stat.execute("alter table test add constraint x primary key(id)");
        ResultSet rs;
        rs = conn.getMetaData().getIndexInfo(null, null, "TEST", true, false);
        rs.next();
        String old = rs.getString("INDEX_NAME");
        stat.execute("alter index " + old + " rename to y");
        rs = conn.getMetaData().getIndexInfo(null, null, "TEST", true, false);
        rs.next();
        assertEquals("Y", rs.getString("INDEX_NAME"));
        reconnect();
        rs = conn.getMetaData().getIndexInfo(null, null, "TEST", true, false);
        rs.next();
        assertEquals("Y", rs.getString("INDEX_NAME"));
        stat.execute("drop table test");
    }

    private void testRandomized() throws SQLException {
        boolean reopen = !config.memory;
        Random rand = new Random(1);
        reconnect();
        stat.execute("drop all objects");
        stat.execute("CREATE TABLE TEST(ID identity)");
        int len = getSize(100, 1000);
        for (int i = 0; i < len; i++) {
            switch (rand.nextInt(4)) {
            case 0:
                if (rand.nextInt(10) == 0) {
                    if (reopen) {
                        trace("reconnect");
                        reconnect();
                    }
                }
                break;
            case 1:
                trace("insert");
                stat.execute("insert into test(id) values(null)");
                break;
            case 2:
                trace("delete");
                stat.execute("delete from test");
                break;
            case 3:
                trace("insert 1-100");
                stat.execute("insert into test select null from system_range(1, 100)");
                break;
            }
        }
        stat.execute("drop table test");
    }

    private void testHashIndex() throws SQLException {
        reconnect();
        stat.execute("create table testA(id int primary key, name varchar)");
        stat.execute("create table testB(id int primary key hash, name varchar)");
        int len = getSize(300, 3000);
        stat.execute("insert into testA select x, 'Hello' from system_range(1, " + len + ")");
        stat.execute("insert into testB select x, 'Hello' from system_range(1, " + len + ")");
        Random rand = new Random(1);
        for (int i = 0; i < len; i++) {
            int x = rand.nextInt(len);
            String sql = "";
            switch(rand.nextInt(3)) {
            case 0:
                sql = "delete from testA where id = " + x;
                break;
            case 1:
                sql = "update testA set name = " + rand.nextInt(100) + " where id = " + x;
                break;
            case 2:
                sql = "select name from testA where id = " + x;
                break;
            default:
            }
            boolean result = stat.execute(sql);
            if (result) {
                ResultSet rs = stat.getResultSet();
                String s1 = rs.next() ? rs.getString(1) : null;
                rs = stat.executeQuery(sql.replace('A', 'B'));
                String s2 = rs.next() ? rs.getString(1) : null;
                assertEquals(s1, s2);
            } else {
                int count1 = stat.getUpdateCount();
                int count2 = stat.executeUpdate(sql.replace('A', 'B'));
                assertEquals(count1, count2);
            }
        }
        stat.execute("drop table testA, testB");
        conn.close();
    }

    private void reconnect() throws SQLException {
        if (conn != null) {
            conn.close();
            conn = null;
        }
        conn = getConnection("index");
        stat = conn.createStatement();
    }

    private void testDescIndex() throws SQLException {
        if (config.memory) {
            return;
        }
        ResultSet rs;
        reconnect();
        stat.execute("CREATE TABLE TEST(ID INT)");
        stat.execute("CREATE INDEX IDX_ND ON TEST(ID DESC)");
        rs = conn.getMetaData().getIndexInfo(null, null, "TEST", false, false);
        rs.next();
        assertEquals("D", rs.getString("ASC_OR_DESC"));
        assertEquals(SortOrder.DESCENDING, rs.getInt("SORT_TYPE"));
        stat.execute("INSERT INTO TEST SELECT X FROM SYSTEM_RANGE(1, 30)");
        rs = stat.executeQuery("SELECT COUNT(*) FROM TEST WHERE ID BETWEEN 10 AND 20");
        rs.next();
        assertEquals(11, rs.getInt(1));
        reconnect();
        rs = conn.getMetaData().getIndexInfo(null, null, "TEST", false, false);
        rs.next();
        assertEquals("D", rs.getString("ASC_OR_DESC"));
        assertEquals(SortOrder.DESCENDING, rs.getInt("SORT_TYPE"));
        rs = stat.executeQuery("SELECT COUNT(*) FROM TEST WHERE ID BETWEEN 10 AND 20");
        rs.next();
        assertEquals(11, rs.getInt(1));
        stat.execute("DROP TABLE TEST");

        stat.execute("create table test(x int, y int)");
        stat.execute("insert into test values(1, 1), (1, 2)");
        stat.execute("create index test_x_y on test (x desc, y desc)");
        rs = stat.executeQuery("select * from test where x=1 and y<2");
        assertTrue(rs.next());

        conn.close();
    }

    private String getRandomString(int len) {
        StringBuilder buff = new StringBuilder();
        for (int i = 0; i < len; i++) {
            buff.append((char) ('a' + random.nextInt(26)));
        }
        return buff.toString();
    }

    private void testWideIndex(int length) throws SQLException {
        reconnect();
        stat.execute("drop all objects");
        stat.execute("CREATE TABLE TEST(ID INT, NAME VARCHAR)");
        stat.execute("CREATE INDEX IDXNAME ON TEST(NAME)");
        for (int i = 0; i < 100; i++) {
            stat.execute("INSERT INTO TEST VALUES(" + i + ", SPACE(" + length + ") || " + i + " )");
        }
        ResultSet rs = stat.executeQuery("SELECT * FROM TEST ORDER BY NAME");
        while (rs.next()) {
            int id = rs.getInt("ID");
            String name = rs.getString("NAME");
            assertEquals("" + id, name.trim());
        }
        if (!config.memory) {
            reconnect();
            rs = stat.executeQuery("SELECT * FROM TEST ORDER BY NAME");
            while (rs.next()) {
                int id = rs.getInt("ID");
                String name = rs.getString("NAME");
                assertEquals("" + id, name.trim());
            }
        }
        stat.execute("drop all objects");
    }

    private void testLike() throws SQLException {
        reconnect();
        stat.execute("CREATE TABLE ABC(ID INT, NAME VARCHAR)");
        stat.execute("INSERT INTO ABC VALUES(1, 'Hello')");
        PreparedStatement prep = conn.prepareStatement("SELECT * FROM ABC WHERE NAME LIKE CAST(? AS VARCHAR)");
        prep.setString(1, "Hi%");
        prep.execute();
        stat.execute("DROP TABLE ABC");
    }

    private void testConstraint() throws SQLException {
        if (config.memory) {
            return;
        }
        stat.execute("CREATE TABLE PARENT(ID INT PRIMARY KEY)");
        stat.execute("CREATE TABLE CHILD(ID INT PRIMARY KEY, PID INT, FOREIGN KEY(PID) REFERENCES PARENT(ID))");
        reconnect();
        stat.execute("DROP TABLE PARENT");
        stat.execute("DROP TABLE CHILD");
    }

    private void testLargeIndex() throws SQLException {
        random.setSeed(10);
        for (int i = 1; i < 100; i += getSize(1000, 7)) {
            stat.execute("DROP TABLE IF EXISTS TEST");
            stat.execute("CREATE TABLE TEST(NAME VARCHAR(" + i + "))");
            stat.execute("CREATE INDEX IDXNAME ON TEST(NAME)");
            PreparedStatement prep = conn.prepareStatement("INSERT INTO TEST VALUES(?)");
            for (int j = 0; j < getSize(2, 5); j++) {
                prep.setString(1, getRandomString(i));
                prep.execute();
            }
            if (!config.memory) {
                conn.close();
                conn = getConnection("index");
                stat = conn.createStatement();
            }
            ResultSet rs = stat.executeQuery("SELECT COUNT(*) FROM TEST WHERE NAME > 'mdd'");
            rs.next();
            int count = rs.getInt(1);
            trace(i + " count=" + count);
        }

        stat.execute("DROP TABLE IF EXISTS TEST");
    }

    private void testHashIndex(boolean primaryKey, boolean hash) throws SQLException {
        if (config.memory) {
            return;
        }

        reconnect();

        stat.execute("DROP TABLE IF EXISTS TEST");
        if (primaryKey) {
            stat.execute("CREATE TABLE TEST(A INT PRIMARY KEY " + (hash ? "HASH" : "") + ", B INT)");
        } else {
            stat.execute("CREATE TABLE TEST(A INT, B INT)");
            stat.execute("CREATE UNIQUE " + (hash ? "HASH" : "") + " INDEX ON TEST(A)");
        }
        PreparedStatement prep;
        prep = conn.prepareStatement("INSERT INTO TEST VALUES(?, ?)");
        int len = getSize(5, 1000);
        for (int a = 0; a < len; a++) {
            prep.setInt(1, a);
            prep.setInt(2, a);
            prep.execute();
            assertEquals(1, getValue("SELECT COUNT(*) FROM TEST WHERE A=" + a));
            assertEquals(0, getValue("SELECT COUNT(*) FROM TEST WHERE A=-1-" + a));
        }

        reconnect();

        prep = conn.prepareStatement("DELETE FROM TEST WHERE A=?");
        for (int a = 0; a < len; a++) {
            if (getValue("SELECT COUNT(*) FROM TEST WHERE A=" + a) != 1) {
                assertEquals(1, getValue("SELECT COUNT(*) FROM TEST WHERE A=" + a));
            }
            prep.setInt(1, a);
            assertEquals(1, prep.executeUpdate());
        }
        assertEquals(0, getValue("SELECT COUNT(*) FROM TEST"));
    }

    private void testMultiColumnIndex() throws SQLException {
        stat.execute("DROP TABLE IF EXISTS TEST");
        stat.execute("CREATE TABLE TEST(A INT, B INT)");
        PreparedStatement prep;
        prep = conn.prepareStatement("INSERT INTO TEST VALUES(?, ?)");
        int len = getSize(3, 260);
        for (int a = 0; a < len; a++) {
            prep.setInt(1, a);
            prep.setInt(2, a);
            prep.execute();
        }
        stat.execute("INSERT INTO TEST SELECT A, B FROM TEST");
        stat.execute("CREATE INDEX ON TEST(A, B)");
        prep = conn.prepareStatement("DELETE FROM TEST WHERE A=?");
        for (int a = 0; a < len; a++) {
            log("SELECT * FROM TEST");
            assertEquals(2, getValue("SELECT COUNT(*) FROM TEST WHERE A=" + (len - a - 1)));
            assertEquals((len - a) * 2, getValue("SELECT COUNT(*) FROM TEST"));
            prep.setInt(1, len - a - 1);
            prep.execute();
        }
        assertEquals(0, getValue("SELECT COUNT(*) FROM TEST"));
    }

    private void testMultiColumnHashIndex() throws SQLException {
        if (config.memory) {
            return;
        }

        stat.execute("DROP TABLE IF EXISTS TEST");
        stat.execute("CREATE TABLE TEST(A INT, B INT, DATA VARCHAR(255))");
        stat.execute("CREATE UNIQUE HASH INDEX IDX_AB ON TEST(A, B)");
        PreparedStatement prep;
        prep = conn.prepareStatement("INSERT INTO TEST VALUES(?, ?, ?)");
        // speed is quadratic (len*len)
        int len = getSize(2, 14);
        for (int a = 0; a < len; a++) {
            for (int b = 0; b < len; b += 2) {
                prep.setInt(1, a);
                prep.setInt(2, b);
                prep.setString(3, "i(" + a + "," + b + ")");
                prep.execute();
            }
        }

        reconnect();

        prep = conn.prepareStatement("UPDATE TEST SET DATA=DATA||? WHERE A=? AND B=?");
        for (int a = 0; a < len; a++) {
            for (int b = 0; b < len; b += 2) {
                prep.setString(1, "u(" + a + "," + b + ")");
                prep.setInt(2, a);
                prep.setInt(3, b);
                prep.execute();
            }
        }

        reconnect();

        ResultSet rs = stat.executeQuery("SELECT * FROM TEST WHERE DATA <> 'i('||a||','||b||')u('||a||','||b||')'");
        assertFalse(rs.next());
        assertEquals(len * (len / 2), getValue("SELECT COUNT(*) FROM TEST"));
        stat.execute("DROP TABLE TEST");
    }

    private int getValue(String sql) throws SQLException {
        ResultSet rs = stat.executeQuery(sql);
        rs.next();
        return rs.getInt(1);
    }

    private void log(String sql) throws SQLException {
        trace(sql);
        ResultSet rs = stat.executeQuery(sql);
        int cols = rs.getMetaData().getColumnCount();
        while (rs.next()) {
            StringBuilder buff = new StringBuilder();
            for (int i = 0; i < cols; i++) {
                if (i > 0) {
                    buff.append(", ");
                }
                buff.append("[" + i + "]=" + rs.getString(i + 1));
            }
            trace(buff.toString());
        }
        trace("---done---");
    }

}

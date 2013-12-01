package table;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 01.12.13
 * Time: 21:37
 */
public class QTableFactory {
    public static QTable createQTable() {
        return new QTableImpl();
    }
}

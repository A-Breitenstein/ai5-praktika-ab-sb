package aufgabe1.manager;

/**
 * User: Alex
 * Date: 06.10.13
 * Time: 00:38
 */
public class KundeCriteria {

    String nachname;

    private KundeCriteria(String nachname) {
        this.nachname = nachname;
    }

    public static KundeCriteria create(String nachname) {
        return new KundeCriteria(nachname);
    }

    public String getNachname() {
        return nachname;
    }

    public boolean hasNachname() {
        return !(getNachname() == null);
    }
}

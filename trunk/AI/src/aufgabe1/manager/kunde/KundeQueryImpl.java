package aufgabe1.manager.kunde;

/**
 * User: Alex
 * Date: 06.10.13
 * Time: 00:38
 */
class KundeQueryImpl implements KundeQuery{

    String nachname;

    private KundeQueryImpl(String nachname) {
        this.nachname = nachname;
    }

    static KundeQueryImpl create(String nachname) {
        return new KundeQueryImpl(nachname);
    }

    public String getNachname() {
        return nachname;
    }

    public boolean hasNachname() {
        return !(getNachname() == null);
    }
}

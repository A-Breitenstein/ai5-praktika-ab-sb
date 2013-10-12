package aufgabe1.manager.kinosaal;

/**
 * User: Alex
 * Date: 06.10.13
 * Time: 00:38
 */
class KinosaalQueryImpl implements KinosaalQuery {

    int kinosaalNr;

    private KinosaalQueryImpl(int kinosaalNr) {
        this.kinosaalNr = kinosaalNr;
    }

    static KinosaalQueryImpl create(int kinosaalNr) {
        return new KinosaalQueryImpl(kinosaalNr);
    }

    @Override
    public int getSaalNr() {
        return kinosaalNr;
    }
}

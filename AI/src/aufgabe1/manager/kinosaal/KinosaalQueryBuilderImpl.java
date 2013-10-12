package aufgabe1.manager.kinosaal;

/**
 * Date: 12.10.13
 * Time: 00:44
 */
class KinosaalQueryBuilderImpl implements KinosaalQueryBuilder {

    private int kinosaalNr;

    private KinosaalQueryBuilderImpl() {
    }

    static KinosaalQueryBuilderImpl create() {
        return new KinosaalQueryBuilderImpl();
    }

    @Override
    public KinosaalQueryBuilder withLastname(int kinosaalNr) {
        this.kinosaalNr = kinosaalNr;
        return this;
    }

    @Override
    public KinosaalQuery createQuery() {
        return KinosaalQueryImpl.create(kinosaalNr);
    }
}

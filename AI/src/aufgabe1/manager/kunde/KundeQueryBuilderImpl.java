package aufgabe1.manager.kunde;

/**
 * Date: 12.10.13
 * Time: 00:44
 */
class KundeQueryBuilderImpl implements KundeQueryBuilder{

    private String lastname;

    private KundeQueryBuilderImpl() {
    }

    static KundeQueryBuilderImpl create() {
        return new KundeQueryBuilderImpl();
    }

    @Override
    public KundeQueryBuilder withLastname(String lastname) {
        this.lastname = lastname;
        return this;
    }

    @Override
    public KundeQuery createQuery() {
        return KundeQueryImpl.create(lastname);
    }
}

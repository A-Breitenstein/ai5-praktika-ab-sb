package aufgabe1.manager.kinosaal;

/**
 * Created with IntelliJ IDEA.
 * User: Akatsuki
 * Date: 12.10.13
 * Time: 00:48
 * To change this template use File | Settings | File Templates.
 */
public interface KinosaalQueryBuilder {

    public KinosaalQueryBuilder withLastname(int kinosaalNr);

    public KinosaalQuery createQuery();
}

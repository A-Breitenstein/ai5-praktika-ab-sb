package aufgabe1.manager.kunde;

/**
 * Created with IntelliJ IDEA.
 * User: Akatsuki
 * Date: 12.10.13
 * Time: 00:48
 * To change this template use File | Settings | File Templates.
 */
public interface KundeQueryBuilder {

    public KundeQueryBuilder withLastname(String lastname);

    public KundeQuery createQuery();
}

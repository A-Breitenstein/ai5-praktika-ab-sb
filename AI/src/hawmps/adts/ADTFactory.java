package hawmps.adts;

import hawmps.adts.fachliche.Adresse;
import hawmps.adts.fachliche.Nummer;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 03.11.13
 * Time: 13:05
 */
final public class ADTFactory {

    public static Nummer Nummer(int nummer) {
        return Nummer.create(nummer);
    }

    public static Adresse Adresse(String strasse, String ort, String plz) {
        return Adresse.create(strasse, ort, plz);
    }


}

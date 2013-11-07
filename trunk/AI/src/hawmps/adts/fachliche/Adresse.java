package hawmps.adts.fachliche;

import javax.persistence.Entity;
import javax.persistence.Id;
import java.io.Serializable;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 03.11.13
 * Time: 12:45
 */
public class Adresse implements Serializable {

    private String strasse;

    private String ort;

    private String plz;

    public Adresse() {
    }

    public static Adresse create(String strasse, String ort, String plz) {
        return new Adresse(strasse, ort, plz);
    }

    @Override
    public String toString() {
        return "Adresse{" +
                "strasse='" + strasse + '\'' +
                ", ort='" + ort + '\'' +
                ", plz='" + plz + '\'' +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Adresse adresse = (Adresse) o;

        if (!ort.equals(adresse.ort)) return false;
        if (!plz.equals(adresse.plz)) return false;
        if (!strasse.equals(adresse.strasse)) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = strasse.hashCode();
        result = 31 * result + ort.hashCode();
        result = 31 * result + plz.hashCode();
        return result;
    }

    private Adresse(String strasse, String ort, String plz) {
        this.strasse = strasse;
        this.ort = ort;
        this.plz = plz;
    }

    public String getStrasse() {
        return strasse;
    }

    public String getOrt() {
        return ort;
    }

    public String getPlz() {
        return plz;
    }

    public void setStrasse(String strasse) {
        this.strasse = strasse;
    }

    public void setOrt(String ort) {
        this.ort = ort;
    }

    public void setPlz(String plz) {
        this.plz = plz;
    }
}

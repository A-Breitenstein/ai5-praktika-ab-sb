package hawmps.komponenten.kundenkomponente.data_access;

import hawmps.adts.fachliche.Adresse;
import hawmps.adts.fachliche.Name;

import java.io.Serializable;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 03.11.13
 * Time: 13:39
 */
public class KundeDTO implements Serializable{
    private int nummer;

    private Name vorname;

    private Name nachname;

    private Adresse adresse;

    public KundeDTO(int nummer, Name vorname, Name nachname, Adresse adresse) {
        this.nummer = nummer;
        this.vorname = vorname;
        this.nachname = nachname;
        this.adresse = adresse;
    }

    public int getNummer() {
        return nummer;
    }

    public void setNummer(int nummer) {
        this.nummer = nummer;
    }

    public Name getVorname() {
        return vorname;
    }

    public void setVorname(Name vorname) {
        this.vorname = vorname;
    }

    public Name getNachname() {
        return nachname;
    }

    public void setNachname(Name nachname) {
        this.nachname = nachname;
    }

    public Adresse getAdresse() {
        return adresse;
    }

    public void setAdresse(Adresse adresse) {
        this.adresse = adresse;
    }
}

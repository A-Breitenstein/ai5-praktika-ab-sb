package hawmps.komponenten.kunden.data_access;

import hawmps.adts.fachliche.Adresse;
import hawmps.adts.fachliche.Name;
import hawmps.adts.fachliche.Nummer;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import javax.persistence.*;
import java.io.Serializable;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 03.11.13
 * Time: 12:36
 */
@Entity
public class Kunde implements Serializable{

    @Id
    @GeneratedValue
    private int nummer;

    private Name vorname;

    private Name nachname;

    private Adresse adresse;

    private Kunde(Name vorname, Name nachname, Adresse adresse) {
        this.vorname = vorname;
        this.nachname = nachname;
        this.adresse = adresse;
    }

    public Kunde() {
    }

    public static Kunde create(Name vorname, Name nachname, Adresse adresse) {
        return new Kunde(vorname, nachname, adresse);
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

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Kunde kunde = (Kunde) o;

        if (!adresse.equals(kunde.adresse)) return false;
        if (!nachname.equals(kunde.nachname)) return false;
//        if (!nummer.equals(kunde.nummer)) return false;
        if (!vorname.equals(kunde.vorname)) return false;

        return true;
    }

//    @Override
//    public int hashCode() {
//        return nummer.hashCode();
//    }

    @Override
    public String toString() {
        return "Kunde{" +
                "nummer=" + nummer +
                ", vorname=" + vorname +
                ", nachname=" + nachname +
                ", adresse=" + adresse +
                '}';
    }

    public KundeDTO toDTO(){
        throw new NotImplementedException();
    }
    public void FromDTO(KundeDTO kundeDTO){
        throw new NotImplementedException();
    }

    public int getNummer() {
        return nummer;
    }
}

package hawmps.komponenten.kunden.data_access;

import hawmps.adts.fachliche.Adresse;
import hawmps.adts.fachliche.Name;
import hawmps.adts.fachliche.Nummer;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.OneToOne;
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
    @OneToOne(cascade = CascadeType.ALL, mappedBy = "Kunde")
    private Nummer nummer;

    @OneToOne
    private Name vorname;

    @OneToOne
    private Name nachname;

    @OneToOne
    private Adresse adresse;

    public Kunde(Nummer nummer, Name vorname, Name nachname, Adresse adresse) {
        this.nummer = nummer;
        this.vorname = vorname;
        this.nachname = nachname;
        this.adresse = adresse;
    }

    public Kunde() {
    }

    public Nummer getNummer() {
        return nummer;
    }

    public void setNummer(Nummer nummer) {
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

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Kunde kunde = (Kunde) o;

        if (!adresse.equals(kunde.adresse)) return false;
        if (!nachname.equals(kunde.nachname)) return false;
        if (!nummer.equals(kunde.nummer)) return false;
        if (!vorname.equals(kunde.vorname)) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return nummer.hashCode();
    }

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

}

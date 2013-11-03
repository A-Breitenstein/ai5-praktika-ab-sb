package hawmps.komponenten.auftraege.data_access;

import hawmps.adts.fachliche.Datum;
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
 * Time: 14:20
 */
@Entity
public class Auftrag implements Serializable{
    @Id
    @OneToOne(cascade = CascadeType.ALL, mappedBy = "Auftrag")
    private Nummer nummer;

    private boolean istAbgeschlossen;
    @OneToOne
    private Datum beauftragtAm;

    @OneToOne
    private Nummer fertigungsAuftragsNummer;

    @OneToOne
    private Nummer angebotsNummer;

    @OneToOne
    private Nummer RechnungsNummer;

    @OneToOne
    private Nummer LieferNummer;

    public Auftrag(Nummer nummer, boolean istAbgeschlossen, Datum beauftragtAm, Nummer fertigungsAuftragsNummer, Nummer angebotsNummer, Nummer rechnungsNummer, Nummer lieferNummer) {
        this.nummer = nummer;
        this.istAbgeschlossen = istAbgeschlossen;
        this.beauftragtAm = beauftragtAm;
        this.fertigungsAuftragsNummer = fertigungsAuftragsNummer;
        this.angebotsNummer = angebotsNummer;
        RechnungsNummer = rechnungsNummer;
        LieferNummer = lieferNummer;
    }

    public Auftrag() {
    }

    public Nummer getNummer() {
        return nummer;
    }

    public void setNummer(Nummer nummer) {
        this.nummer = nummer;
    }

    public boolean isIstAbgeschlossen() {
        return istAbgeschlossen;
    }

    public void setIstAbgeschlossen(boolean istAbgeschlossen) {
        this.istAbgeschlossen = istAbgeschlossen;
    }

    public Datum getBeauftragtAm() {
        return beauftragtAm;
    }

    public void setBeauftragtAm(Datum beauftragtAm) {
        this.beauftragtAm = beauftragtAm;
    }

    public Nummer getFertigungsAuftragsNummer() {
        return fertigungsAuftragsNummer;
    }

    public void setFertigungsAuftragsNummer(Nummer fertigungsAuftragsNummer) {
        this.fertigungsAuftragsNummer = fertigungsAuftragsNummer;
    }

    public Nummer getAngebotsNummer() {
        return angebotsNummer;
    }

    public void setAngebotsNummer(Nummer angebotsNummer) {
        this.angebotsNummer = angebotsNummer;
    }

    public Nummer getRechnungsNummer() {
        return RechnungsNummer;
    }

    public void setRechnungsNummer(Nummer rechnungsNummer) {
        RechnungsNummer = rechnungsNummer;
    }

    public Nummer getLieferNummer() {
        return LieferNummer;
    }

    public void setLieferNummer(Nummer lieferNummer) {
        LieferNummer = lieferNummer;
    }

    @Override
    public String toString() {
        return "Auftrag{" +
                "nummer=" + nummer +
                ", istAbgeschlossen=" + istAbgeschlossen +
                ", beauftragtAm=" + beauftragtAm +
                ", fertigungsAuftragsNummer=" + fertigungsAuftragsNummer +
                ", angebotsNummer=" + angebotsNummer +
                ", RechnungsNummer=" + RechnungsNummer +
                ", LieferNummer=" + LieferNummer +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Auftrag auftrag = (Auftrag) o;

        if (!nummer.equals(auftrag.nummer)) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return nummer.hashCode();
    }

    public AuftragDTO toDTO(){
        throw new NotImplementedException();
    }
    public void fromDTO(AuftragDTO auftragDTO){
        throw new NotImplementedException();
    }
}

package hawmps.komponenten.auftragskomponente.data_access;

import hawmps.adts.fachliche.Datum;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import javax.persistence.*;
import java.io.Serializable;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 03.11.13
 * Time: 14:20
 */
@Entity
public class Auftrag implements Serializable{

    @Id
    private int nummer;

    private boolean istAbgeschlossen;

    private Datum beauftragtAm;

    @OneToMany
    private List<FertigungsAuftrag> zugehoerigeFertigungsAuftrage;

    private int angebotsNummer;

    private int RechnungsNummer;

    private int LieferNummer;

    private Auftrag(boolean istAbgeschlossen, Datum beauftragtAm, List<FertigungsAuftrag> zugehoerigeFertigungsAuftrage, int angebotsNummer, int rechnungsNummer, int lieferNummer) {
        this.istAbgeschlossen = istAbgeschlossen;
        this.beauftragtAm = beauftragtAm;
        this.zugehoerigeFertigungsAuftrage = zugehoerigeFertigungsAuftrage;
        this.angebotsNummer = angebotsNummer;
        RechnungsNummer = rechnungsNummer;
        LieferNummer = lieferNummer;
    }

    public Auftrag() {
    }

    public static Auftrag create(boolean istAbgeschlossen, Datum beauftragtAm, List<FertigungsAuftrag> zugehoerigeFertigungsAuftrage, int angebotsNummer, int rechnungsNummer, int lieferNummer) {
        return new Auftrag(istAbgeschlossen, beauftragtAm, zugehoerigeFertigungsAuftrage, angebotsNummer, rechnungsNummer, lieferNummer);
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

    public List<FertigungsAuftrag> getZugehoerigeFertigungsAuftrage() {
        return zugehoerigeFertigungsAuftrage;
    }

    public void setZugehoerigeFertigungsAuftrage(List<FertigungsAuftrag> zugehoerigeFertigungsAuftrage) {
        this.zugehoerigeFertigungsAuftrage = zugehoerigeFertigungsAuftrage;
    }

    public void setNummer(int nummer) {
        this.nummer = nummer;
    }

    public int getAngebotsNummer() {
        return angebotsNummer;
    }

    public void setAngebotsNummer(int angebotsNummer) {
        this.angebotsNummer = angebotsNummer;
    }

    public int getRechnungsNummer() {
        return RechnungsNummer;
    }

    public void setRechnungsNummer(int rechnungsNummer) {
        RechnungsNummer = rechnungsNummer;
    }

    public int getLieferNummer() {
        return LieferNummer;
    }

    public void setLieferNummer(int lieferNummer) {
        LieferNummer = lieferNummer;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Auftrag auftrag = (Auftrag) o;

//        if (!nummer.equals(auftrag.nummer)) return false;

        return true;
    }

//    @Override
//    public int hashCode() {
//        return nummer.hashCode();
//    }

    public AuftragDTO toDTO(){
        throw new NotImplementedException();
    }
    public void fromDTO(AuftragDTO auftragDTO){
        throw new NotImplementedException();
    }

    @Override
    public String toString() {
        return "Auftrag{" +
                "nummer=" + nummer +
                ", istAbgeschlossen=" + istAbgeschlossen +
                ", beauftragtAm=" + beauftragtAm +
                ", zugehoerigeFertigungsAuftrage=" + zugehoerigeFertigungsAuftrage +
                ", angebotsNummer=" + angebotsNummer +
                ", RechnungsNummer=" + RechnungsNummer +
                ", LieferNummer=" + LieferNummer +
                '}';
    }

    public int getNummer() {
        return nummer;
    }
}

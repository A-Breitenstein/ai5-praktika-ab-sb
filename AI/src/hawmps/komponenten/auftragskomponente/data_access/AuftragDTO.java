package hawmps.komponenten.auftragskomponente.data_access;

import hawmps.adts.fachliche.Datum;

import java.io.Serializable;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 03.11.13
 * Time: 14:49
 */
public class AuftragDTO implements Serializable {
    private int nummer;

    private boolean istAbgeschlossen;

    private Datum beauftragtAm;

    private List<FertigungsAuftragDTO> zugehoerigeFertigungsAuftrage;

    private int angebotsNummer;

    private int RechnungsNummer;

    private int LieferNummer;

    public AuftragDTO(int nummer, boolean istAbgeschlossen, Datum beauftragtAm, List<FertigungsAuftragDTO> zugehoerigeFertigungsAuftrage, int angebotsNummer, int rechnungsNummer, int lieferNummer) {
        this.nummer = nummer;
        this.istAbgeschlossen = istAbgeschlossen;
        this.beauftragtAm = beauftragtAm;
        this.zugehoerigeFertigungsAuftrage = zugehoerigeFertigungsAuftrage;
        this.angebotsNummer = angebotsNummer;
        RechnungsNummer = rechnungsNummer;
        LieferNummer = lieferNummer;
    }

    public int getNummer() {
        return nummer;
    }

    public void setNummer(int nummer) {
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

    public List<FertigungsAuftragDTO> getZugehoerigeFertigungsAuftrage() {
        return zugehoerigeFertigungsAuftrage;
    }

    public void setZugehoerigeFertigungsAuftrage(List<FertigungsAuftragDTO> zugehoerigeFertigungsAuftrage) {
        this.zugehoerigeFertigungsAuftrage = zugehoerigeFertigungsAuftrage;
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
    public String toString() {
        return "AuftragDTO{" +
                "nummer=" + nummer +
                ", istAbgeschlossen=" + istAbgeschlossen +
                ", beauftragtAm=" + beauftragtAm +
                ", zugehoerigeFertigungsAuftrage=" + zugehoerigeFertigungsAuftrage +
                ", angebotsNummer=" + angebotsNummer +
                ", RechnungsNummer=" + RechnungsNummer +
                ", LieferNummer=" + LieferNummer +
                '}';
    }
}

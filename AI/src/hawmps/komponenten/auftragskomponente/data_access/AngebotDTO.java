package hawmps.komponenten.auftragskomponente.data_access;

import hawmps.adts.fachliche.Betrag;
import hawmps.adts.fachliche.Datum;

import java.io.Serializable;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 24.11.13
 * Time: 13:46
 */
public class AngebotDTO implements Serializable {
    private int nummer;
    private Datum gueltigAb;
    private Datum gueltigBis;
    private Betrag preis;
    private int kundenNummer;
    private int bauteilNummer;
    private AuftragDTO auftragDTO;

    public AngebotDTO(int nummer, Datum gueltigAb, Datum gueltigBis, Betrag preis, int kundenNummer, int bauteilNummer, AuftragDTO auftragDTO) {
        this.nummer = nummer;
        this.gueltigAb = gueltigAb;
        this.gueltigBis = gueltigBis;
        this.preis = preis;
        this.kundenNummer = kundenNummer;
        this.bauteilNummer = bauteilNummer;
        this.auftragDTO = auftragDTO;
    }

    public int getNummer() {
        return nummer;
    }

    public void setNummer(int nummer) {
        this.nummer = nummer;
    }

    public Datum getGueltigAb() {
        return gueltigAb;
    }

    public void setGueltigAb(Datum gueltigAb) {
        this.gueltigAb = gueltigAb;
    }

    public Datum getGueltigBis() {
        return gueltigBis;
    }

    public void setGueltigBis(Datum gueltigBis) {
        this.gueltigBis = gueltigBis;
    }

    public Betrag getPreis() {
        return preis;
    }

    public void setPreis(Betrag preis) {
        this.preis = preis;
    }

    public int getKundenNummer() {
        return kundenNummer;
    }

    public void setKundenNummer(int kundenNummer) {
        this.kundenNummer = kundenNummer;
    }

    public int getBauteilNummer() {
        return bauteilNummer;
    }

    public void setBauteilNummer(int bauteilNummer) {
        this.bauteilNummer = bauteilNummer;
    }

    public AuftragDTO getAuftragDTO() {
        return auftragDTO;
    }

    public void setAuftragDTO(AuftragDTO auftragDTO) {
        this.auftragDTO = auftragDTO;
    }

    @Override
    public String toString() {
        return "AngebotDTO{" +
                "nummer=" + nummer +
                ", gueltigAb=" + gueltigAb +
                ", gueltigBis=" + gueltigBis +
                ", preis=" + preis +
                ", kundenNummer=" + kundenNummer +
                ", bauteilNummer=" + bauteilNummer +
                ", auftragDTO=" + auftragDTO +
                '}';
    }
}

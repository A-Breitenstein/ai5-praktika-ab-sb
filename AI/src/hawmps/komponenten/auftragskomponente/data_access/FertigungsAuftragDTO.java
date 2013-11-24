package hawmps.komponenten.auftragskomponente.data_access;

import java.io.Serializable;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 05.11.13
 * Time: 20:43
 */
public class FertigungsAuftragDTO implements Serializable {
    private int nummer;

    private AuftragDTO gehoertZuAuftrag;

    private int bauteilNummer;

    public FertigungsAuftragDTO(int nummer, AuftragDTO gehoertZuAuftrag, int bauteilNummer) {
        this.nummer = nummer;
        this.gehoertZuAuftrag = gehoertZuAuftrag;
        this.bauteilNummer = bauteilNummer;
    }

    public int getNummer() {
        return nummer;
    }

    public void setNummer(int nummer) {
        this.nummer = nummer;
    }

    public AuftragDTO getGehoertZuAuftrag() {
        return gehoertZuAuftrag;
    }

    public void setGehoertZuAuftrag(AuftragDTO gehoertZuAuftrag) {
        this.gehoertZuAuftrag = gehoertZuAuftrag;
    }

    public int getBauteilNummer() {
        return bauteilNummer;
    }

    public void setBauteilNummer(int bauteilNummer) {
        this.bauteilNummer = bauteilNummer;
    }
}

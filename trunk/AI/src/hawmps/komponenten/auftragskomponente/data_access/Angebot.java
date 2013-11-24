package hawmps.komponenten.auftragskomponente.data_access;

import hawmps.adts.fachliche.Betrag;
import hawmps.adts.fachliche.Datum;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.OneToOne;
import java.io.Serializable;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 24.11.13
 * Time: 13:25
 */
@Entity
public class Angebot implements Serializable {
    @Id
    @GeneratedValue
    private int nummer;

    private Datum gueltigAb;
    private Datum gueltigBis;
    private Betrag preis;

    private int kundenNummer;
    private int bauteilNummer;
    @OneToOne
    private Auftrag auftrag;

    public Angebot() {
    }

    private Angebot(Datum gueltigAb, Datum gueltigBis, Betrag preis, int kundenNummer, int bauteilNummer) {
        this.gueltigAb = gueltigAb;
        this.gueltigBis = gueltigBis;
        this.preis = preis;
        this.kundenNummer = kundenNummer;
        this.bauteilNummer = bauteilNummer;
    }

    public static Angebot create(Datum gueltigAb, Datum gueltigBis, Betrag preis, int kundenNummer, int bauteilNummer) {
        return new Angebot(gueltigAb, gueltigBis, preis, kundenNummer, bauteilNummer);
    }

    public int getNummer() {
        return nummer;
    }

    public void setNummer(int id) {
        this.nummer = id;
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

    public Auftrag getAuftrag() {
        return auftrag;
    }

    public void setAuftrag(Auftrag auftrag) {
        this.auftrag = auftrag;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Angebot angebot = (Angebot) o;

        if (nummer != angebot.nummer) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return nummer;
    }

    @Override
    public String toString() {
        return "Angebot{" +
                "id=" + nummer +
                ", gueltigAb=" + gueltigAb +
                ", gueltigBis=" + gueltigBis +
                ", preis=" + preis +
                ", kundenNummer=" + kundenNummer +
                ", bauteilNummer=" + bauteilNummer +
                ", auftrag=" + auftrag +
                '}';
    }
    public AngebotDTO toDTO() {
        AngebotDTO angebotDTO = new AngebotDTO(nummer, gueltigAb, gueltigBis, preis, kundenNummer, bauteilNummer, null);
        if (auftrag != null) {
            angebotDTO.setAuftragDTO(auftrag.toDTO());
        }
        return angebotDTO;
    }
    public  void fromDTO(AngebotDTO angebotDTO) {
        nummer = angebotDTO.getNummer();
        gueltigAb = angebotDTO.getGueltigAb();
        gueltigBis = angebotDTO.getGueltigBis();
        preis = angebotDTO.getPreis();
        kundenNummer = angebotDTO.getKundenNummer();
        bauteilNummer = angebotDTO.getBauteilNummer();
        if (angebotDTO.getAuftragDTO() != null) {
            auftrag =  new Auftrag();
            auftrag.fromDTO(angebotDTO.getAuftragDTO());
        }

    }

}

package hawmps.komponenten.bauteilkomponente.data_access;

import hawmps.adts.fachliche.Datum;

import java.io.Serializable;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 05.11.13
 * Time: 17:21
 */
public class StuecklisteDTO implements Serializable {
    private int nummer;

    private Datum gueltigAb;

    private Datum gueltigBis;

    private List<StuecklistenPositionDTO> stuecklistenPositionen;

    public StuecklisteDTO(int nummer, Datum gueltigAb, Datum gueltigBis, List<StuecklistenPositionDTO> stuecklistenPositionen) {
        this.nummer = nummer;
        this.gueltigAb = gueltigAb;
        this.gueltigBis = gueltigBis;
        this.stuecklistenPositionen = stuecklistenPositionen;
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

    public List<StuecklistenPositionDTO> getStuecklistenPositionen() {
        return stuecklistenPositionen;
    }

    public void setStuecklistenPositionen(List<StuecklistenPositionDTO> stuecklistenPositionen) {
        this.stuecklistenPositionen = stuecklistenPositionen;
    }

    @Override
    public String toString() {
        return "StuecklisteDTO{" +
                "nummer=" + nummer +
                ", gueltigAb=" + gueltigAb +
                ", gueltigBis=" + gueltigBis +
                ", stuecklistenPositionen=" + stuecklistenPositionen +
                '}';
    }
}

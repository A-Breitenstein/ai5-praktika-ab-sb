package hawmps.komponenten.auftraege.data_access;

import hawmps.adts.fachliche.Nummer;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import javax.persistence.*;
import java.io.Serializable;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 05.11.13
 * Time: 20:24
 */
@Entity
public class FertigungsAuftrag implements Serializable{
    @Id
    @OneToOne(cascade = CascadeType.ALL, mappedBy = "FertigungsAuftrag")
    private Nummer nummer;
    @ManyToOne
    private Auftrag gehoertZuAuftrag;
    @OneToOne
    private Nummer bauteilNummer;

    public FertigungsAuftrag() {
    }

    private FertigungsAuftrag(Auftrag gehoertZuAuftrag, Nummer bauteilNummer) {
        this.gehoertZuAuftrag = gehoertZuAuftrag;
        this.bauteilNummer = bauteilNummer;
    }

    public static FertigungsAuftrag create(Auftrag gehoertZuAuftrag, Nummer bauteilNummer) {
        return new FertigungsAuftrag(gehoertZuAuftrag, bauteilNummer);
    }

    public Nummer getNummer() {
        return nummer;
    }

    public void setNummer(Nummer nummer) {
        this.nummer = nummer;
    }

    public Auftrag getGehoertZuAuftrag() {
        return gehoertZuAuftrag;
    }

    public void setGehoertZuAuftrag(Auftrag gehoertZuAuftrag) {
        this.gehoertZuAuftrag = gehoertZuAuftrag;
    }

    public Nummer getBauteilNummer() {
        return bauteilNummer;
    }

    public void setBauteilNummer(Nummer bauteilNummer) {
        this.bauteilNummer = bauteilNummer;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        FertigungsAuftrag that = (FertigungsAuftrag) o;

        if (!nummer.equals(that.nummer)) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return nummer.hashCode();
    }

    @Override
    public String toString() {
        return "FertigungsAuftrag{" +
                "nummer=" + nummer +
                ", gehoertZuAuftrag=" + gehoertZuAuftrag +
                ", bauteilNummer=" + bauteilNummer +
                '}';
    }

    public FertigungsAuftragDTO toDTO(){
        throw new NotImplementedException();
    }
    public void fromDTO(FertigungsAuftragDTO fertigungsAuftragDTO) {
        throw new NotImplementedException();
    }
}

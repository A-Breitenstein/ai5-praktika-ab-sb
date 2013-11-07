package hawmps.komponenten.auftragskomponente.data_access;

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
    @GeneratedValue
    private int nummer;

    @ManyToOne
    private Auftrag gehoertZuAuftrag;

    private int bauteilNummer;

    public FertigungsAuftrag() {
    }

    private FertigungsAuftrag(Auftrag gehoertZuAuftrag, int bauteilNummer) {
        this.gehoertZuAuftrag = gehoertZuAuftrag;
        this.bauteilNummer = bauteilNummer;
    }

    public static FertigungsAuftrag create(Auftrag gehoertZuAuftrag, int bauteilNummer) {
        return new FertigungsAuftrag(gehoertZuAuftrag, bauteilNummer);
    }

    public Auftrag getGehoertZuAuftrag() {
        return gehoertZuAuftrag;
    }

    public void setGehoertZuAuftrag(Auftrag gehoertZuAuftrag) {
        this.gehoertZuAuftrag = gehoertZuAuftrag;
    }

    public int getNummer() {
        return nummer;
    }

    public void setNummer(int nummer) {
        this.nummer = nummer;
    }

    public int getBauteilNummer() {
        return bauteilNummer;
    }

    public void setBauteilNummer(int bauteilNummer) {
        this.bauteilNummer = bauteilNummer;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        FertigungsAuftrag that = (FertigungsAuftrag) o;

//        if (!nummer.equals(that.nummer)) return false;

        return true;
    }

//    @Override
//    public int hashCode() {
//        return nummer.hashCode();
//    }

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

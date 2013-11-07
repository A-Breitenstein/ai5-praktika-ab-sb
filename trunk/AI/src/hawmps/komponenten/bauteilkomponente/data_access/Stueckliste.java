package hawmps.komponenten.bauteilkomponente.data_access;

import hawmps.adts.fachliche.Datum;

import javax.persistence.*;
import java.io.Serializable;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 04.11.13
 * Time: 20:25
 */
@Entity
public class Stueckliste implements Serializable {

    @Id
    @GeneratedValue
    private int nummer;

    private Datum gueltigAb;

    private Datum gueltigBis;

    @OneToMany(cascade = CascadeType.ALL)
    private List<StuecklistenPosition> stuecklistenPositionen;

    public Stueckliste() {
    }

    private Stueckliste(Datum gueltigAb, Datum gueltigBis, List<StuecklistenPosition> stuecklistenPositionen) {
        this.gueltigAb = gueltigAb;
        this.gueltigBis = gueltigBis;
        this.stuecklistenPositionen = stuecklistenPositionen;
    }

    public static Stueckliste create(Datum gueltigAb, Datum gueltigBis, List<StuecklistenPosition> stuecklistenPositionen) {
        return new Stueckliste(gueltigAb, gueltigBis, stuecklistenPositionen);
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

    public List<StuecklistenPosition> getStuecklistenPositionen() {
        return stuecklistenPositionen;
    }

    public void setStuecklistenPositionen(List<StuecklistenPosition> stuecklistenPositionen) {
        this.stuecklistenPositionen = stuecklistenPositionen;
    }

    @Override
    public String toString() {
        return "Stueckliste{" +
                "nummer=" + nummer +
                ", gueltigAb=" + gueltigAb +
                ", gueltigBis=" + gueltigBis +
                ", stuecklistenPositionen=" + stuecklistenPositionen +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Stueckliste that = (Stueckliste) o;

//        if (!nummer.equals(that.nummer)) return false;

        return true;
    }

//    @Override
//    public int hashCode() {
//        return nummer.hashCode();
//    }
}

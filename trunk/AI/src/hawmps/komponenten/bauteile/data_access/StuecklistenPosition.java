package hawmps.komponenten.bauteile.data_access;

import hawmps.adts.fachliche.Nummer;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.OneToOne;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 04.11.13
 * Time: 20:30
 */
@Entity
final public class StuecklistenPosition {
    @Id
    @OneToOne(cascade = CascadeType.ALL, mappedBy = "StuecklistenPosition")
    private Nummer nummer;
    @OneToOne
    private Nummer menge;
    @OneToOne
    private  Bauteil bauteil;

    public StuecklistenPosition() {
    }

    private StuecklistenPosition(Nummer menge, Bauteil bauteil) {
        this.menge = menge;
        this.bauteil = bauteil;
    }

    public static StuecklistenPosition create(Nummer menge, Bauteil bauteil) {
        return new StuecklistenPosition(menge, bauteil);
    }

    public Nummer getNummer() {
        return nummer;
    }

    public void setNummer(Nummer nummer) {
        this.nummer = nummer;
    }

    public Nummer getMenge() {
        return menge;
    }

    public void setMenge(Nummer menge) {
        this.menge = menge;
    }

    public Bauteil getBauteil() {
        return bauteil;
    }

    public void setBauteil(Bauteil bauteil) {
        this.bauteil = bauteil;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        StuecklistenPosition that = (StuecklistenPosition) o;

        if (nummer != null ? !nummer.equals(that.nummer) : that.nummer != null) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return nummer != null ? nummer.hashCode() : 0;
    }

    @Override
    public String toString() {
        return "StuecklistenPosition{" +
                "nummer=" + nummer +
                ", menge=" + menge +
                ", bauteil=" + bauteil +
                '}';
    }
}

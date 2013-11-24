package hawmps.komponenten.bauteilkomponente.data_access;

import javax.persistence.*;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 04.11.13
 * Time: 20:30
 */
@Entity
public class StuecklistenPosition {

    @Id
    @GeneratedValue
    private int nummer;

    private int menge;

    @OneToOne
    private  Bauteil bauteil;

    public StuecklistenPosition() {
    }

    private StuecklistenPosition(int menge, Bauteil bauteil) {
        this.menge = menge;
        this.bauteil = bauteil;
    }

    public static StuecklistenPosition create(int menge, Bauteil bauteil) {
        return new StuecklistenPosition(menge, bauteil);
    }

    public int getMenge() {
        return menge;
    }

    public void setMenge(int menge) {
        this.menge = menge;
    }

    public int getNummer() {
        return nummer;
    }

    public void setNummer(int nummer) {
        this.nummer = nummer;
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


        return true;
    }


    @Override
    public String toString() {
        return "StuecklistenPosition{" +
                "nummer=" + nummer +
                ", menge=" + menge +
                ", bauteil=" + bauteil +
                '}';
    }
    public StuecklistenPositionDTO toDTO(BauteilDTO bauteilDTO) {
        return new StuecklistenPositionDTO(nummer,menge,bauteilDTO);
    }
    public void fromDTO(StuecklistenPositionDTO stuecklistenPositionDTO,Bauteil bauteil) {
        nummer = stuecklistenPositionDTO.getNummer();
        menge = stuecklistenPositionDTO.getMenge();
        this.bauteil = bauteil;
    }
}

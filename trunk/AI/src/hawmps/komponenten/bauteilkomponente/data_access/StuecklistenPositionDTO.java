package hawmps.komponenten.bauteilkomponente.data_access;

import java.io.Serializable;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 05.11.13
 * Time: 17:21
 */
public class StuecklistenPositionDTO implements Serializable {
    private int nummer;

    private int menge;

    private  BauteilDTO bauteil;

    public StuecklistenPositionDTO(int nummer, int menge, BauteilDTO bauteil) {
        this.nummer = nummer;
        this.menge = menge;
        this.bauteil = bauteil;
    }

    public int getNummer() {
        return nummer;
    }

    public void setNummer(int nummer) {
        this.nummer = nummer;
    }

    public int getMenge() {
        return menge;
    }

    public void setMenge(int menge) {
        this.menge = menge;
    }

    public BauteilDTO getBauteil() {
        return bauteil;
    }

    public void setBauteil(BauteilDTO bauteil) {
        this.bauteil = bauteil;
    }

    @Override
    public String toString() {
        return "StuecklistenPositionDTO{" +
                "nummer=" + nummer +
                ", menge=" + menge +
                ", bauteil=" + bauteil.getName() +
                '}';
    }
}

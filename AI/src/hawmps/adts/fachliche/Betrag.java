package hawmps.adts.fachliche;

import java.io.Serializable;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 24.11.13
 * Time: 13:28
 */
public class Betrag implements Serializable {
    private float betrag;

    public Betrag() {
    }

    private Betrag(float betrag) {
        this.betrag = betrag;
    }

    public static Betrag create(float betrag) {
        return new Betrag(betrag);
    }

    public float getBetrag() {
        return betrag;
    }

    public void setBetrag(float betrag) {
        this.betrag = betrag;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Betrag betrag1 = (Betrag) o;

        if (Float.compare(betrag1.betrag, betrag) != 0) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return (betrag != +0.0f ? Float.floatToIntBits(betrag) : 0);
    }

    @Override
    public String toString() {
        return "Betrag{" +
                "betrag=" + betrag +
                '}';
    }
}

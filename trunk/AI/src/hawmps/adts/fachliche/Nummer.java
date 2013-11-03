package hawmps.adts.fachliche;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import java.io.Serializable;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 03.11.13
 * Time: 12:45
 */
@Entity
final public class Nummer  implements Serializable {
    @Id
    @GeneratedValue
    private int nummer;

    public Nummer() {
    }

    private Nummer(int nummer) {
        this.nummer = nummer;
    }

    public static Nummer create(int nummer) {
        return new Nummer(nummer);
    }

    public int getNummer() {
        return nummer;
    }

    public void setNummer(int nummer) {
        this.nummer = nummer;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Nummer nummer1 = (Nummer) o;

        if (nummer != nummer1.nummer) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return nummer;
    }

    @Override
    public String toString() {
        return "Nummer{" +
                "nummer=" + nummer +
                '}';
    }
}

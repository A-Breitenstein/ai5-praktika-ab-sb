package hawmps.adts.fachliche;

import javax.persistence.Entity;
import javax.persistence.Id;
import java.io.Serializable;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 03.11.13
 * Time: 14:22
 */
@Entity
public class Datum implements Serializable {
    @Id
    private String datum;

    public Datum() {
    }

    private Datum(String datum) {
        this.datum = datum;
    }

    public String getDatum() {
        return datum;
    }

    public void setDatum(String datum) {
        this.datum = datum;
    }

    public static Datum create(String datum) {
        return new Datum(datum);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Datum datum1 = (Datum) o;

        if (!datum.equals(datum1.datum)) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return datum.hashCode();
    }

    @Override
    public String toString() {
        return "Datum{" +
                "datum='" + datum + '\'' +
                '}';
    }
}

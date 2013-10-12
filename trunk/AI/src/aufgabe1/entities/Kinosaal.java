package aufgabe1.entities;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import java.io.Serializable;
import java.util.Set;

/**
 * Date: 04.10.13
 * Time: 16:54
 */
@Entity
public class Kinosaal implements Serializable{

    @Id
    private int SaalNr;

    private int size;

    public Kinosaal() {
    }

    private Kinosaal(int saalNr, int size) {
        SaalNr = saalNr;
        this.size = size;
    }

    public static Kinosaal create(int saalNr, int size) {
        return new Kinosaal(saalNr, size);
    }

    public int getSaalNr() {
        return SaalNr;
    }

    public void setSaalNr(int saalNr) {
        SaalNr = saalNr;
    }

    public int getSize() {
        return size;
    }

    public void setSize(int size) {
        this.size = size;
    }

    @Override
    public String toString() {
        return "Kinosaal{" +
                "SaalNr=" + SaalNr +
                ", size=" + size +
                '}';
    }
}


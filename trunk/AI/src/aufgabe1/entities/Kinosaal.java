package aufgabe1.entities;

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

    @OneToMany
    private Set<Kinokarte> kinokarten;

    public Kinosaal() {
    }

    private Kinosaal(int saalNr, int size, Set<Kinokarte> kinokarten) {
        SaalNr = saalNr;
        this.size = size;
        this.kinokarten = kinokarten;
    }

    public static Kinosaal create(int saalNr, int size, Set<Kinokarte> kinokarten) {
        return new Kinosaal(saalNr, size, kinokarten);
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

    public Set<Kinokarte> getKinokarten() {
        return kinokarten;
    }

    public void setKinokarten(Set<Kinokarte> kinokarten) {
        this.kinokarten = kinokarten;
    }
}


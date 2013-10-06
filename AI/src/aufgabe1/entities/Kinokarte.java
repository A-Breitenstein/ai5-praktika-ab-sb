package aufgabe1.entities;

import javax.persistence.*;
import java.io.Serializable;

/**
 * Date: 04.10.13
 * Time: 16:54
 */
@Entity
public class Kinokarte implements Serializable{

    @Id
    @GeneratedValue
    private int barcode;

    @ManyToOne(cascade = CascadeType.ALL)
    private Kinosaal kinosaal;

    public Kinokarte() {
    }

    private Kinokarte(Kinosaal kinosaal) {
        this.kinosaal = kinosaal;
    }

    public static Kinokarte create(Kinosaal kinosaal) {
        return new Kinokarte(kinosaal);
    }

    public int getBarcode() {
        return barcode;
    }

    public void setBarcode(int barcode) {
        this.barcode = barcode;
    }

    public Kinosaal getKinosaal() {
        return kinosaal;
    }

    public void setKinosaal(Kinosaal kinosaal) {
        this.kinosaal = kinosaal;
    }
}

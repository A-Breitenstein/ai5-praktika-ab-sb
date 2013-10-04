package aufgabe1.entities;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import java.io.Serializable;

/**
 * Date: 04.10.13
 * Time: 16:54
 */
@Entity
public class Kinokarte implements Serializable{

    @Id
    private int barcode;

    @ManyToOne
    private Kunde kunde;

    @ManyToOne
    private Kinosaal kinosaal;

    public Kinokarte() {
    }

    private Kinokarte(int barcode, Kunde kunde, Kinosaal kinosaal) {
        this.barcode = barcode;
        this.kunde = kunde;
        this.kinosaal = kinosaal;
    }

    public static Kinokarte create(int barcode, Kunde kunde, Kinosaal kinosaal) {
        return new Kinokarte(barcode, kunde, kinosaal);
    }

    public int getBarcode() {
        return barcode;
    }

    public void setBarcode(int barcode) {
        this.barcode = barcode;
    }

    public Kunde getKunde() {
        return kunde;
    }

    public void setKunde(Kunde kunde) {
        this.kunde = kunde;
    }

    public Kinosaal getKinosaal() {
        return kinosaal;
    }

    public void setKinosaal(Kinosaal kinosaal) {
        this.kinosaal = kinosaal;
    }
}

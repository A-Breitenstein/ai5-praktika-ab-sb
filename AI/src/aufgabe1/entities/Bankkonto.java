package aufgabe1.entities;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.OneToOne;
import java.io.Serializable;

/**
 * Date: 04.10.13
 * Time: 16:54
 */
@Entity
public class Bankkonto implements Serializable{

    @Id
    private String IBAN;

    @OneToOne
    private Kunde kunde;

    public Bankkonto() {
    }

    private Bankkonto(String IBAN, Kunde kunde) {
        this.IBAN = IBAN;
        this.kunde = kunde;
    }

    public static Bankkonto create(String IBAN, Kunde kunde) {
        return new Bankkonto(IBAN, kunde);
    }

    public String getIBAN() {
        return IBAN;
    }

    public void setIBAN(String IBAN) {
        this.IBAN = IBAN;
    }

    public Kunde getKunde() {
        return kunde;
    }

    public void setKunde(Kunde kunde) {
        this.kunde = kunde;
    }
}

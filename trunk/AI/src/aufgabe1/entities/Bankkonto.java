package aufgabe1.entities;

import javax.persistence.*;
import java.io.Serializable;

/**
 * Date: 04.10.13
 * Time: 16:54
 */
@Entity
public class Bankkonto implements Serializable{

    @Id
    @GeneratedValue
    private int id;

    private String IBAN;

    public Bankkonto() {
    }

    private Bankkonto(String IBAN) {
        this.IBAN = IBAN;
    }

    public static Bankkonto create(String IBAN) {
        return new Bankkonto(IBAN);
    }

    public String getIBAN() {
        return IBAN;
    }

    public void setIBAN(String IBAN) {
        this.IBAN = IBAN;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    @Override
    public String toString() {
        return "Bankkonto{" +
                "id=" + id +
                ", IBAN='" + IBAN + '\'' +
                '}';
    }
}

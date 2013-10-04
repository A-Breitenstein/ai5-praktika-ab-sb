package aufgabe1.entities;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.OneToOne;
import java.io.Serializable;

/**
 * Created with IntelliJ IDEA.
 * User: Akatsuki
 * Date: 04.10.13
 * Time: 16:54
 * To change this template use File | Settings | File Templates.
 */
@Entity
public class Bankkonto implements Serializable{

    @Id
    private String IBAN;

    @OneToOne
    private Kunde kunde;

    public Bankkonto() {
    }

}

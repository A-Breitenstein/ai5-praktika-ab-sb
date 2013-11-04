package hawmps.komponenten.bauteile.data_access;

import hawmps.adts.fachliche.Name;
import hawmps.adts.fachliche.Nummer;

import javax.persistence.*;
import java.io.Serializable;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 04.11.13
 * Time: 20:17
 */
@Entity
final public class Bauteil implements Serializable{
    @Id
    @OneToOne(cascade = CascadeType.ALL, mappedBy = "Bauteil")
    private Nummer nummer;
    @OneToOne
    private Name name;
    @OneToOne
    private Nummer arbeitsplanNummer;
    @OneToOne
    private Stueckliste stueckliste;

}

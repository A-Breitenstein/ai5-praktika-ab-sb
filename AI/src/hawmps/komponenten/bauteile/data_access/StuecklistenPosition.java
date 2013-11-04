package hawmps.komponenten.bauteile.data_access;

import hawmps.adts.fachliche.Nummer;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.OneToOne;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 04.11.13
 * Time: 20:30
 */
@Entity
final public class StuecklistenPosition {
    @Id
    @OneToOne(cascade = CascadeType.ALL, mappedBy = "StuecklistenPosition")
    private Nummer nummer;
    @OneToOne
    private Nummer menge;
    @OneToOne
    private  Bauteil bauteil;
}

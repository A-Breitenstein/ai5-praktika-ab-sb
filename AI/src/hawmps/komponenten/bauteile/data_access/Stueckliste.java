package hawmps.komponenten.bauteile.data_access;

import hawmps.adts.fachliche.Datum;
import hawmps.adts.fachliche.Nummer;

import javax.persistence.*;
import java.io.Serializable;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 04.11.13
 * Time: 20:25
 */
@Entity
final public class Stueckliste implements Serializable {
    @Id
    @OneToOne(cascade = CascadeType.ALL, mappedBy = "Stueckliste")
    private Nummer nummer;
    @OneToOne
    private Datum gueltigAb;
    @OneToOne
    private Datum gueltigBis;
    @OneToMany(cascade = CascadeType.ALL)
    private List<StuecklistenPosition> stuecklistenPositionen;

}

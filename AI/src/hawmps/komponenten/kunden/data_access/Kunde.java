package hawmps.komponenten.kunden.data_access;

import hawmps.adts.fachliche.Adresse;
import hawmps.adts.fachliche.Name;
import hawmps.adts.fachliche.Nummer;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.OneToOne;
import java.io.Serializable;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 03.11.13
 * Time: 12:36
 */
@Entity
public class Kunde implements Serializable{

    @OneToOne(cascade = CascadeType.ALL)
    private Nummer nummer;

    @OneToOne
    private Name vorname;

    @OneToOne
    private Name nachname;

    @OneToOne
    private Adresse adresse;


}

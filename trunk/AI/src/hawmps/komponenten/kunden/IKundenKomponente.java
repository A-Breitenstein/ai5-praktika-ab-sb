package hawmps.komponenten.kunden;

import hawmps.adts.fachliche.Adresse;
import hawmps.adts.fachliche.Name;
import hawmps.komponenten.kunden.data_access.Kunde;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 03.11.13
 * Time: 13:38
 */
public interface IKundenKomponente {
    Kunde createKunde(Name name,Adresse adresse);
    List<Kunde> findByKundenName(Name Kundename);
}

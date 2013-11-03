package hawmps.komponenten.kunden.access;

import hawmps.adts.fachliche.Adresse;
import hawmps.adts.fachliche.Name;
import hawmps.komponenten.kunden.IKundenKomponente;
import hawmps.komponenten.kunden.data_access.Kunde;
import hawmps.komponenten.kunden.data_access.Repository;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 03.11.13
 * Time: 13:47
 */
public class KundenKomponente implements IKundenKomponente {

    private Repository repository;

    @Override
    public Kunde createKunde(Name name, Adresse adresse) {
        throw new UnsupportedOperationException("KundenKomponente :: createKunde not implemented yet");
    }

    @Override
    public List<Kunde> findByKundenName(Name Kundename) {
        throw new UnsupportedOperationException("KundenKomponente :: findByKundenName not implemented yet");
    }
}

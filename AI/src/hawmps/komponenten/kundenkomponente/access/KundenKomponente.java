package hawmps.komponenten.kundenkomponente.access;

import hawmps.adts.fachliche.Adresse;
import hawmps.adts.fachliche.Name;
import hawmps.komponenten.kundenkomponente.IKundenKomponente;
import hawmps.komponenten.kundenkomponente.data_access.Kunde;
import hawmps.komponenten.kundenkomponente.data_access.Repository;

import javax.persistence.EntityManager;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 03.11.13
 * Time: 13:47
 */
public class KundenKomponente implements IKundenKomponente {

    private Repository repository;
    private EntityManager entityManager;

    private KundenKomponente(EntityManager entityManager) {
        this.entityManager = entityManager;
        this.repository = Repository.create(entityManager);
    }

    public static KundenKomponente create(EntityManager entityManager) {
        return new KundenKomponente(entityManager);
    }

    @Override
    public Kunde createKunde(Name Vorname, Name Nachname, Adresse adresse) {
        return repository.createKunde(Vorname,Nachname,adresse);
    }

    @Override
    public List<Kunde> findByNachname(Name Nachname) {
        return repository.findKundeByNachname(Nachname);
    }
}

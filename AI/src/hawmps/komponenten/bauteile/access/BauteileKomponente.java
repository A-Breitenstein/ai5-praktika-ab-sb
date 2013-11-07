package hawmps.komponenten.bauteile.access;

import hawmps.adts.fachliche.Name;
import hawmps.adts.fachliche.Nummer;
import hawmps.komponenten.bauteile.IBauteileKomponente;
import hawmps.komponenten.bauteile.data_access.Bauteil;
import hawmps.komponenten.bauteile.data_access.Repository;
import hawmps.komponenten.bauteile.data_access.Stueckliste;

import javax.persistence.EntityManager;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 05.11.13
 * Time: 17:15
 */
public class BauteileKomponente implements IBauteileKomponente {
    private Repository repository;
    private EntityManager entityManager;

    private BauteileKomponente(EntityManager entityManager) {
        this.repository = Repository.create(entityManager);
        this.entityManager = entityManager;
    }

    public static BauteileKomponente create(EntityManager entityManager) {
        return new BauteileKomponente(entityManager);
    }

    @Override
    public Bauteil createBauteil(Name name, int arbeitsplanNummer, Stueckliste stueckliste) {
        return repository.createBauteil(name, arbeitsplanNummer, stueckliste);
    }

    @Override
    public void updateBauteil(Bauteil bauteil) {
        repository.updateBauteil(bauteil);
    }

    @Override
    public void deleteBauteilByNummer(int BauteilNummer) {
        repository.deleteBauteilByNummer(BauteilNummer);
    }

    @Override
    public Bauteil findBauteilByNummer(int nummer) {
        return repository.findBauteilByNummer(nummer);
    }

    @Override
    public List<Bauteil> findBauteilByName(Name name) {
        return repository.findBauteilByName(name);
    }
}

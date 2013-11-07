package hawmps.komponenten.bauteile.data_access;

import hawmps.adts.fachliche.Name;
import hawmps.adts.fachliche.Nummer;

import javax.persistence.EntityManager;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 04.11.13
 * Time: 20:17
 */
public class Repository {
    private EntityManager entityManager;

    private Repository(EntityManager entityManager) {
        this.entityManager = entityManager;
    }

    public static Repository create(EntityManager entityManager) {
        return new Repository(entityManager);
    }

    public Bauteil createBauteil(Name name, int arbeitsplanNummer, Stueckliste stueckliste){
        Bauteil bauteil = Bauteil.create(name, arbeitsplanNummer, stueckliste);
        entityManager.persist(bauteil);
        return  bauteil;
    }
    public void updateBauteil(Bauteil bauteil){
        entityManager.merge(bauteil);
    }

    public void deleteBauteilByNummer(int BauteilNummer) {
        Bauteil bauteil = findBauteilByNummer(BauteilNummer);
        entityManager.remove(bauteil);
    }
    public Bauteil findBauteilByNummer(int nummer){
        CriteriaBuilder builder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Bauteil> query = builder.createQuery(Bauteil.class);
        Root<Bauteil> root = query.from(Bauteil.class);

        query.select(root).where(builder.equal(root.get("nummer"), nummer));
        List<Bauteil> Bauteile = new ArrayList<Bauteil>(entityManager.createQuery(query).getResultList());
        return Bauteile.get(0);
    }
    public List<Bauteil> findBauteilByName(Name name) {
        CriteriaBuilder builder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Bauteil> query = builder.createQuery(Bauteil.class);
        Root<Bauteil> root = query.from(Bauteil.class);

        query.select(root).where(builder.equal(root.get("name"), name));
        List<Bauteil> Bauteile = new ArrayList<Bauteil>(entityManager.createQuery(query).getResultList());
        return Bauteile;
    }

}

package hawmps.komponenten.kundenkomponente.data_access;

import hawmps.adts.fachliche.Adresse;
import hawmps.adts.fachliche.Name;

import javax.persistence.EntityManager;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 03.11.13
 * Time: 13:49
 */
public class Repository {
    private EntityManager entityManager;

    private Repository(EntityManager entityManager) {
        this.entityManager = entityManager;
    }

    public static Repository create(EntityManager entityManager) {
        return new Repository(entityManager);
    }

    public Kunde createKunde(Name vorname, Name nachname, Adresse adresse){
        Kunde kunde = Kunde.create(vorname, nachname, adresse);
        entityManager.persist(kunde);
        return kunde;
    }
    public void updateKunde(Kunde kunde) {
        entityManager.merge(kunde);
    }
    public void deleteKunde(int kundenNummer){
        Kunde derKunde = findKundeByNummer(kundenNummer);
        entityManager.remove(derKunde);
    }

    public List<Kunde> findKundeByNachname(Name Nachname){
        CriteriaBuilder builder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Kunde> query = builder.createQuery(Kunde.class);
        Root<Kunde> root = query.from(Kunde.class);

        query.select(root).where(builder.equal(root.get("nachname"), Nachname));
        List<Kunde> Kunde = new ArrayList<Kunde>(entityManager.createQuery(query).getResultList());
        return Kunde;
    }
    public Kunde findKundeByNummer(int kundenNummer){
        CriteriaBuilder builder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Kunde> query = builder.createQuery(Kunde.class);
        Root<Kunde> root = query.from(Kunde.class);

        query.select(root).where(builder.equal(root.get("nummer"), kundenNummer));
        List<Kunde> Kunde = new ArrayList<Kunde>(entityManager.createQuery(query).getResultList());
        return Kunde.get(0);
    }
}

package aufgabe1.manager;

import aufgabe1.entities.Kunde;
import aufgabe1.persistence.PersistenceUtilsA1;

import javax.persistence.EntityManager;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import java.util.ArrayList;
import java.util.List;

/**
 * Date: 04.10.13
 * Time: 18:47
 */
public class KundeManager {

    private static EntityManager entityManager;

    public static void create(Kunde kunde) {
        entityManager = PersistenceUtilsA1.createEntityManager();

        entityManager.getTransaction().begin();
        entityManager.persist(kunde);
        entityManager.getTransaction().commit();

        entityManager.close();
    }

    public static void update(Kunde kunde) {
        entityManager = PersistenceUtilsA1.createEntityManager();

        entityManager.getTransaction().begin();
        entityManager.merge(kunde);
        entityManager.getTransaction().commit();

        entityManager.close();
    }

    public static void delete(KundeCriteria kundeCriteria) {
        entityManager = PersistenceUtilsA1.createEntityManager();

        entityManager.getTransaction().begin();

        CriteriaBuilder builder = entityManager.getCriteriaBuilder();

        CriteriaQuery<Kunde> query = builder.createQuery(Kunde.class);

        Root<Kunde> root = query.from(Kunde.class);

        List<Predicate> predicates = new ArrayList<Predicate>();

        if (kundeCriteria.hasNachname()) {
            predicates.add(builder.equal(root.get("nachname"),kundeCriteria.getNachname()));
        }

        List<Kunde> kunde = new ArrayList<Kunde>(entityManager.createQuery(query).getResultList());


        entityManager.remove(kunde.get(0));

        entityManager.getTransaction().commit();

        entityManager.close();
    }

    public static List<Kunde> find(KundeCriteria kundeCriteria) {
        entityManager = PersistenceUtilsA1.createEntityManager();

        CriteriaBuilder builder = entityManager.getCriteriaBuilder();

        CriteriaQuery<Kunde> query = builder.createQuery(Kunde.class);

        Root<Kunde> root = query.from(Kunde.class);

        List<Predicate> predicates = new ArrayList<Predicate>();

        if (kundeCriteria.hasNachname()) {
            predicates.add(builder.equal(root.get("nachname"),kundeCriteria.getNachname()));
        }

        List<Kunde> kunde = new ArrayList<Kunde>(entityManager.createQuery(query).getResultList());

        entityManager.close();

        return kunde;
    }
}

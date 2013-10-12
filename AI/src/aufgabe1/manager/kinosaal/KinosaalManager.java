package aufgabe1.manager.kinosaal;

import aufgabe1.entities.Kinosaal;
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
 * Time: 18:46
 */
public class KinosaalManager {

    private static EntityManager entityManager;

    public static KinosaalQueryBuilder createQueryBuilder() {
        return KinosaalQueryBuilderImpl.create();
    }

    public static void create(Kinosaal kinosaal) {
        entityManager = PersistenceUtilsA1.createEntityManager();

        entityManager.getTransaction().begin();
        entityManager.persist(kinosaal);
        entityManager.getTransaction().commit();

        entityManager.close();
    }

    public static List<Kinosaal> find(KinosaalQuery kinosaalQuery) {
        entityManager = PersistenceUtilsA1.createEntityManager();

        CriteriaBuilder builder = entityManager.getCriteriaBuilder();

        CriteriaQuery<Kinosaal> query = builder.createQuery(Kinosaal.class);

        Root<Kinosaal> root = query.from(Kinosaal.class);

        List<Predicate> predicates = new ArrayList<Predicate>();

        if (kinosaalQuery.getSaalNr() > 0) {
            predicates.add(builder.equal(root.get("SaalNr"),kinosaalQuery.getSaalNr()));
        }

        query.select(root).where(predicates.toArray(new Predicate[predicates.size()]));

        List<Kinosaal> kinosaalList = new ArrayList<Kinosaal>(entityManager.createQuery(query).getResultList());

        entityManager.close();

        return kinosaalList;
    }

    public static void update(Kinosaal kinosaal) {

    }

    public static void delete(Kinosaal kinosaal) {

    }
}

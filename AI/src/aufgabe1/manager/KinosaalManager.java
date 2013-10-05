package aufgabe1.manager;

import aufgabe1.entities.Kinosaal;
import aufgabe1.persistence.PersistenceUtilsA1;

import javax.persistence.EntityManager;

/**
 * Date: 04.10.13
 * Time: 18:46
 */
public class KinosaalManager {

    private static EntityManager entityManager;

    public static void create(Kinosaal kinosaal) {
        entityManager = PersistenceUtilsA1.createEntityManager();

        entityManager.getTransaction().begin();
        entityManager.merge(kinosaal);
        entityManager.getTransaction().commit();

        entityManager.close();
    }

    public static void update(Kinosaal kinosaal) {

    }

    public static void delete(Kinosaal kinosaal) {

    }
}

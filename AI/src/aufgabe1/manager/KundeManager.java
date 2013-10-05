package aufgabe1.manager;

import aufgabe1.entities.Kunde;
import aufgabe1.persistence.PersistenceUtilsA1;

import javax.persistence.EntityManager;
import javax.persistence.Persistence;

/**
 * Date: 04.10.13
 * Time: 18:47
 */
public class KundeManager {

    private static EntityManager entityManager;


    public static void create(Kunde kunde) {
        entityManager = PersistenceUtilsA1.createEntityManager();

        entityManager.getTransaction().begin();
        entityManager.merge(kunde);
        entityManager.getTransaction().commit();

        entityManager.close();
    }

    public static void update(Kunde kunde) {

    }

    public static void delete(Kunde kunde) {

    }

    public static Kunde find(Kunde kunde) {
        entityManager = PersistenceUtilsA1.createEntityManager();

        Object o = entityManager.createNativeQuery("select * from kunde where nachname like '" + kunde.getNachname()+"'").getSingleResult();

        entityManager.close();

        return kunde;
    }
}

package aufgabe1.persistence;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

/**
 * Date: 04.10.13
 * Time: 19:10
 */
public class PersistenceUtilsA1 {
    public static String PersistenceUnitName = "NewPersistenceUnit";
    public static EntityManagerFactory emf= Persistence.createEntityManagerFactory(PersistenceUtilsA1.PersistenceUnitName);

    public static EntityManager createEntityManager() {
        return emf.createEntityManager();
    }

    public static void main(String[] args) {
        createEntityManager();
    }

}

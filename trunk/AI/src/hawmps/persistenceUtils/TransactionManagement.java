package hawmps.persistenceUtils;

import hawmps.HawMpsMain;

import javax.persistence.EntityManager;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 24.11.13
 * Time: 18:29
 */
public class TransactionManagement {
    EntityManager entityManager;

    public TransactionManagement(EntityManager entityManager) {
        this.entityManager = entityManager;
    }
    public void beginTransaction() {
      entityManager.getTransaction().begin();
    }
    public void commitTransaction() {
      entityManager.getTransaction().commit();
    }
    public void rollback() {
        entityManager.getTransaction().rollback();
    }
}

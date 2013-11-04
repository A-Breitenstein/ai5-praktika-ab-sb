package hawmps;

import aufgabe1.persistence.PersistenceUtilsA1;
import hawmps.adts.fachliche.Adresse;
import hawmps.adts.fachliche.Name;
import hawmps.komponenten.kunden.IKundenKomponente;
import hawmps.komponenten.kunden.access.KundenKomponente;
import hawmps.komponenten.kunden.data_access.Kunde;
import junit.framework.Assert;
import org.junit.Before;
import org.testng.annotations.Test;

import javax.persistence.EntityManager;


/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 04.11.13
 * Time: 20:55
 */
public class KundenKomponenteTests {
    IKundenKomponente kundenKomponente;
    EntityManager entityManager;

    @Before
    public void startUpCode() {
        entityManager = PersistenceUtilsA1.createEntityManager();
        kundenKomponente = KundenKomponente.create(entityManager);
    }

    @Test
    public void createKunde(){
        entityManager.getTransaction().begin();
        Kunde kunde = kundenKomponente.createKunde(Name.create("Sven"), Name.create("Bartel"), Adresse.create("qwe str1","hh","22457"));
        Assert.assertTrue(kundenKomponente.findByNachname(Name.create("Bartel")).size() > 0);
        entityManager.getTransaction().commit();
    }
}

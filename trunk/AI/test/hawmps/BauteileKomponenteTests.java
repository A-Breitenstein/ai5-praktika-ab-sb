package hawmps;

import aufgabe1.persistence.PersistenceUtilsA1;
import hawmps.adts.fachliche.Datum;
import hawmps.adts.fachliche.Name;
import hawmps.komponenten.bauteilkomponente.IBauteileKomponente;
import hawmps.komponenten.bauteilkomponente.access.BauteileKomponente;
import hawmps.komponenten.bauteilkomponente.data_access.Bauteil;
import hawmps.komponenten.bauteilkomponente.data_access.BauteilDTO;
import hawmps.komponenten.bauteilkomponente.data_access.Stueckliste;
import hawmps.komponenten.bauteilkomponente.data_access.StuecklistenPosition;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.testng.annotations.Test;

import javax.persistence.EntityManager;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 05.11.13
 * Time: 17:18
 */
public class BauteileKomponenteTests {
    IBauteileKomponente bauteileKomponente;
    EntityManager entityManager;

    public BauteileKomponenteTests() {
        startUpCode();
    }

    @Override
    protected void finalize() throws Throwable {
        cleanUpCode();
        super.finalize();    //To change body of overridden methods use File | Settings | File Templates.
    }

    public void startUpCode() {
        entityManager = PersistenceUtilsA1.createEntityManager();
        bauteileKomponente = BauteileKomponente.create(entityManager);

        entityManager.getTransaction().begin();
    }
    public void cleanUpCode(){
        entityManager.getTransaction().commit();
    }

    @Test
    public void createKomplexBauteil() throws RemoteException {
        Name name = Name.create("Tisch");
        bauteileKomponente.createTestBauteil(name);
        BauteilDTO tischAusDB = bauteileKomponente.findBauteilByName(name).get(0);
        Assert.assertTrue(tischAusDB != null);
    }
}

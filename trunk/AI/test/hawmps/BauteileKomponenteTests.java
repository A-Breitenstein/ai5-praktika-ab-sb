package hawmps;

import aufgabe1.persistence.PersistenceUtilsA1;
import hawmps.adts.fachliche.Datum;
import hawmps.adts.fachliche.Name;
import hawmps.adts.fachliche.Nummer;
import hawmps.komponenten.bauteile.IBauteileKomponente;
import hawmps.komponenten.bauteile.access.BauteileKomponente;
import hawmps.komponenten.bauteile.data_access.Bauteil;
import hawmps.komponenten.bauteile.data_access.Stueckliste;
import hawmps.komponenten.bauteile.data_access.StuecklistenPosition;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.testng.annotations.Test;

import javax.persistence.EntityManager;
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

    @Before
    public void startUpCode() {
        entityManager = PersistenceUtilsA1.createEntityManager();
        bauteileKomponente = BauteileKomponente.create(entityManager);

        entityManager.getTransaction().begin();
    }

    @After
    public void cleanUpCode(){
        entityManager.getTransaction().commit();
    }

    @Test
    public void createKomplexBauteil() {
        Bauteil schraube = bauteileKomponente.createBauteil(Name.create("Schraube AB3"),-1,null);
        Bauteil kantholz = bauteileKomponente.createBauteil(Name.create("Kantholz XYZ3"),-1,null);
        Bauteil mutter = bauteileKomponente.createBauteil(Name.create("Mutter AB3"),-1,null);
        Bauteil holzplatte = bauteileKomponente.createBauteil(Name.create("Holzplatte"),-1,null);



        List<StuecklistenPosition> stuecklistenPositionen = new ArrayList<StuecklistenPosition>();
        stuecklistenPositionen.add(StuecklistenPosition.create(25, schraube));
        stuecklistenPositionen.add(StuecklistenPosition.create(4, kantholz));
        stuecklistenPositionen.add(StuecklistenPosition.create(25, mutter));
        stuecklistenPositionen.add(StuecklistenPosition.create(1, holzplatte));

        Stueckliste stueckliste = Stueckliste.create(Datum.create("20.11.13"), Datum.create("25.11.14"), stuecklistenPositionen);

        Bauteil tisch = bauteileKomponente.createBauteil(Name.create("Tisch"),-1,stueckliste);

        Bauteil tischAusDB = bauteileKomponente.findBauteilByName(Name.create("Tisch")).get(0);
        Assert.assertTrue(tisch.getNummer()==(tischAusDB.getNummer()));
    }
}

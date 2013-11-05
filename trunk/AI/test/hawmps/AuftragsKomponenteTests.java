package hawmps;

import aufgabe1.persistence.PersistenceUtilsA1;
import hawmps.adts.fachliche.Datum;
import hawmps.adts.fachliche.Nummer;
import hawmps.komponenten.auftraege.IAuftragsKomponente;
import hawmps.komponenten.auftraege.access.AuftragsKomponente;
import hawmps.komponenten.auftraege.data_access.Auftrag;
import hawmps.komponenten.bauteile.access.BauteileKomponente;
import hawmps.komponenten.kunden.access.KundenKomponente;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.testng.annotations.Test;

import javax.persistence.EntityManager;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 04.11.13
 * Time: 21:06
 */
public class AuftragsKomponenteTests {
    IAuftragsKomponente auftragsKomponente;
    EntityManager entityManager;

    @Before
    public void startUpCode() {
        entityManager = PersistenceUtilsA1.createEntityManager();

        auftragsKomponente = AuftragsKomponente.create(entityManager,
                                        BauteileKomponente.create(entityManager),
                                        KundenKomponente.create(entityManager)
                            );

        entityManager.getTransaction().begin();
    }

    @After
    public void cleanUpCode(){
        entityManager.getTransaction().commit();
    }

    @Test
    public void createAuftrag_find_delete(){
        Auftrag auftrag = auftragsKomponente.createAuftrag(true, Datum.create("10.10.2013"),null,null,null,null);
        Assert.assertTrue(auftragsKomponente.findAuftragByNummer(auftrag.getNummer()).equals(auftrag));
        auftragsKomponente.deleteAuftragByNummer(auftrag.getNummer());
        Assert.assertTrue(auftragsKomponente.findAuftragByNummer(auftrag.getNummer()) == null);
    }
    @Test
    public void updateAuftrag(){
        Auftrag auftrag = auftragsKomponente.createAuftrag(false, Datum.create("11.11.2013"),null,null,null,null);
        auftrag.setAngebotsNummer(Nummer.create(1));
        auftragsKomponente.updateAuftrag(auftrag);

        Auftrag auftragnochmal = auftragsKomponente.findAuftragByNummer(auftrag.getNummer());
        Assert.assertTrue(auftragnochmal.getAngebotsNummer().equals(Nummer.create(1)));

        auftragsKomponente.deleteAuftragByNummer(auftragnochmal.getNummer());
    }
}

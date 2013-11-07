package hawmps;

import aufgabe1.persistence.PersistenceUtilsA1;
import hawmps.adts.fachliche.Datum;
import hawmps.adts.fachliche.Name;
import hawmps.komponenten.auftragskomponente.IAuftragsKomponente;
import hawmps.komponenten.auftragskomponente.access.AuftragsKomponente;
import hawmps.komponenten.auftragskomponente.data_access.Auftrag;
import hawmps.komponenten.bauteilkomponente.IBauteileKomponente;
import hawmps.komponenten.bauteilkomponente.access.BauteileKomponente;
import hawmps.komponenten.bauteilkomponente.data_access.Bauteil;
import hawmps.komponenten.bauteilkomponente.data_access.Stueckliste;
import hawmps.komponenten.bauteilkomponente.data_access.StuecklistenPosition;
import hawmps.komponenten.kundenkomponente.IKundenKomponente;
import hawmps.komponenten.kundenkomponente.access.KundenKomponente;
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
 * Date: 04.11.13
 * Time: 21:06
 */
public class AuftragsKomponenteTests {
    IAuftragsKomponente auftragsKomponente;
    IBauteileKomponente bauteileKomponente;
    IKundenKomponente kundenKomponente;
    EntityManager entityManager;

    public AuftragsKomponenteTests() {
        startUpCode();
    }

    @Override
    protected void finalize() throws Throwable {
        cleanUpCode();
        super.finalize();    //To change body of overridden methods use File | Settings | File Templates.
    }

    public void startUpCode() {
        entityManager = PersistenceUtilsA1.createEntityManager();
        kundenKomponente = KundenKomponente.create(entityManager);
        bauteileKomponente = BauteileKomponente.create(entityManager);

        auftragsKomponente = AuftragsKomponente.create(
                                    entityManager,
                                    bauteileKomponente,
                                    kundenKomponente
                             );

        entityManager.getTransaction().begin();
    }

    public void cleanUpCode(){
        entityManager.getTransaction().commit();
    }

    @Test
    public void createAuftrag_find_delete(){
        Auftrag auftrag = auftragsKomponente.createAuftrag(true, Datum.create("10.10.2013"),null,-1,-1,-1);
        Assert.assertTrue(auftragsKomponente.findAuftragByNummer(auftrag.getNummer()).equals(auftrag));
        auftragsKomponente.deleteAuftragByNummer(auftrag.getNummer());
        Assert.assertTrue(auftragsKomponente.findAuftragByNummer(auftrag.getNummer()) == null);
    }
    @Test
    public void updateAuftrag(){
        Auftrag auftrag = auftragsKomponente.createAuftrag(false, Datum.create("11.11.2013"),null,-1,-1,-1);
        auftrag.setAngebotsNummer(1);
        auftragsKomponente.updateAuftrag(auftrag);

        Auftrag auftragnochmal = auftragsKomponente.findAuftragByNummer(auftrag.getNummer());
        Assert.assertTrue(auftragnochmal.getAngebotsNummer() == 1);

        auftragsKomponente.deleteAuftragByNummer(auftragnochmal.getNummer());
    }

    @Test
    public void uebuerfuehreAngebotInAuftrag() {
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

        Auftrag neuerAuftrag = auftragsKomponente.ueberfuehreAngebotInAuftrag(tisch.getNummer());

        Assert.assertTrue(neuerAuftrag.getZugehoerigeFertigungsAuftrage().size() == 1);
        Assert.assertTrue(auftragsKomponente.findAuftragByNummer(neuerAuftrag.getNummer()).getNummer() == neuerAuftrag.getNummer());


    }
}

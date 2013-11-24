package hawmps;

import aufgabe1.persistence.PersistenceUtilsA1;
import hawmps.adts.fachliche.Adresse;
import hawmps.adts.fachliche.Betrag;
import hawmps.adts.fachliche.Datum;
import hawmps.adts.fachliche.Name;
import hawmps.komponenten.auftragskomponente.IAuftragsKomponente;
import hawmps.komponenten.auftragskomponente.access.AuftragsKomponente;
import hawmps.komponenten.auftragskomponente.data_access.AngebotDTO;
import hawmps.komponenten.auftragskomponente.data_access.Auftrag;
import hawmps.komponenten.auftragskomponente.data_access.AuftragDTO;
import hawmps.komponenten.bauteilkomponente.IBauteileKomponente;
import hawmps.komponenten.bauteilkomponente.access.BauteileKomponente;
import hawmps.komponenten.bauteilkomponente.data_access.Bauteil;
import hawmps.komponenten.bauteilkomponente.data_access.BauteilDTO;
import hawmps.komponenten.bauteilkomponente.data_access.Stueckliste;
import hawmps.komponenten.bauteilkomponente.data_access.StuecklistenPosition;
import hawmps.komponenten.kundenkomponente.IKundenKomponente;
import hawmps.komponenten.kundenkomponente.access.KundenKomponente;
import hawmps.komponenten.kundenkomponente.data_access.KundeDTO;
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
        AuftragDTO auftragDTO = auftragsKomponente.createAuftrag(true, Datum.create("10.10.2013"),null,-1,-1,-1);
        Assert.assertTrue(auftragsKomponente.findAuftragByNummer(auftragDTO.getNummer()).getNummer() == auftragDTO.getNummer());
        auftragsKomponente.deleteAuftragByNummer(auftragDTO.getNummer());
        Assert.assertTrue(auftragsKomponente.findAuftragByNummer(auftragDTO.getNummer()) == null);
    }
    @Test
    public void updateAuftrag(){
        AuftragDTO auftragDTO = auftragsKomponente.createAuftrag(false, Datum.create("11.11.2013"),null,-1,-1,-1);
        auftragDTO.setAngebotsNummer(1);
        auftragsKomponente.updateAuftrag(auftragDTO);

        AuftragDTO auftragDTOnochmal = auftragsKomponente.findAuftragByNummer(auftragDTO.getNummer());
        Assert.assertTrue(auftragDTOnochmal.getAngebotsNummer() == 1);

        auftragsKomponente.deleteAuftragByNummer(auftragDTOnochmal.getNummer());
    }

    @Test
    public void uebuerfuehreAngebotInAuftrag() {
        Name name = Name.create("Tisch");
        bauteileKomponente.createTestBauteil(name);
        BauteilDTO tisch = bauteileKomponente.findBauteilByName(name).get(0);
        KundeDTO kunde1 = kundenKomponente.createKunde(Name.create("Hans"), Name.create("Meiser"), Adresse.create("dd","aa","bb"));

        AngebotDTO angebotDTO = auftragsKomponente.createAngebot(Datum.create("heute"), Datum.create("bis morgen"), Betrag.create(5555),kunde1.getNummer(), tisch.getNummer());
        AuftragDTO neuerAuftrag = auftragsKomponente.ueberfuehreAngebotInAuftrag(angebotDTO);

        Assert.assertTrue(neuerAuftrag.getZugehoerigeFertigungsAuftrage().size() == 1);
        Assert.assertTrue(auftragsKomponente.findAuftragByNummer(neuerAuftrag.getNummer()).getNummer() == neuerAuftrag.getNummer());


    }
}

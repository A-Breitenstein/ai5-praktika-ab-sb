package hawmps.komponenten.auftragskomponente.access;

import hawmps.adts.fachliche.Datum;
import hawmps.komponenten.auftragskomponente.IAuftragsKomponente;
import hawmps.komponenten.auftragskomponente.business.AuftragsVerwaltung;
import hawmps.komponenten.auftragskomponente.data_access.Auftrag;
import hawmps.komponenten.auftragskomponente.data_access.FertigungsAuftrag;
import hawmps.komponenten.auftragskomponente.data_access.Repository;
import hawmps.komponenten.bauteilkomponente.IBauteileKomponente;
import hawmps.komponenten.kundenkomponente.IKundenKomponente;

import javax.persistence.EntityManager;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 03.11.13
 * Time: 15:35
 */
public class AuftragsKomponente implements IAuftragsKomponente {
    private Repository repository;
    private EntityManager entityManager;
    private AuftragsVerwaltung auftragsVerwaltung;
    private IKundenKomponente kundenKomponente;
    private IBauteileKomponente bauteileKomponente;

    private AuftragsKomponente(EntityManager entityManager, IBauteileKomponente bauteileKomponente, IKundenKomponente kundenKomponente) {
        this.repository = Repository.create(entityManager);
        this.bauteileKomponente = bauteileKomponente;
        this.kundenKomponente = kundenKomponente;
        this.auftragsVerwaltung = AuftragsVerwaltung.create(repository,bauteileKomponente,kundenKomponente);
        this.entityManager = entityManager;
    }

    public static AuftragsKomponente create(EntityManager entityManager, IBauteileKomponente bauteileKomponente, IKundenKomponente kundenKomponente) {
        return new AuftragsKomponente(entityManager, bauteileKomponente, kundenKomponente);
    }


    @Override
    public Auftrag createAuftrag(boolean istAbgeschlossen, Datum beauftragtAm, List<FertigungsAuftrag> zugehoerigeFertigungsAuftrage, int angebotsNummer, int rechnungsNummer, int lieferNummer) {
        return repository.createAuftrag(istAbgeschlossen, beauftragtAm, zugehoerigeFertigungsAuftrage, angebotsNummer, rechnungsNummer, lieferNummer);
    }

    @Override
    public void updateAuftrag(Auftrag auftrag) {
        repository.updateAuftrag(auftrag);
    }

    @Override
    public void deleteAuftragByNummer(int auftragsNummer) {
        repository.deleteAuftragByNummer(auftragsNummer);
    }

    @Override
    public Auftrag findAuftragByNummer(int auftragsNummer) {
        return repository.findAuftragByNummer(auftragsNummer);
    }

    @Override
    public Auftrag ueberfuehreAngebotInAuftrag(int bauteilNummer) {
        return auftragsVerwaltung.ueberfuehreAngebotInAuftrag(bauteilNummer);
    }

}

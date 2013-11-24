package hawmps.komponenten.auftragskomponente.access;

import hawmps.adts.fachliche.Betrag;
import hawmps.adts.fachliche.Datum;
import hawmps.komponenten.auftragskomponente.IAuftragsKomponente;
import hawmps.komponenten.auftragskomponente.business.AuftragsVerwaltung;
import hawmps.komponenten.auftragskomponente.data_access.*;
import hawmps.komponenten.bauteilkomponente.IBauteileKomponente;
import hawmps.komponenten.kundenkomponente.IKundenKomponente;

import javax.persistence.EntityManager;
import java.util.ArrayList;
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
    public AuftragDTO createAuftrag(boolean istAbgeschlossen, Datum beauftragtAm, List<FertigungsAuftrag> zugehoerigeFertigungsAuftrage, int angebotsNummer, int rechnungsNummer, int lieferNummer) {
        return repository.createAuftrag(istAbgeschlossen, beauftragtAm, zugehoerigeFertigungsAuftrage, angebotsNummer, rechnungsNummer, lieferNummer).toDTO();
    }

    @Override
    public void updateAuftrag(AuftragDTO auftragDTO) {
        Auftrag auftrag = new Auftrag();
        auftrag.fromDTO(auftragDTO);
        repository.updateAuftrag(auftrag);
    }

    @Override
    public void deleteAuftragByNummer(int auftragsNummer) {
        repository.deleteAuftragByNummer(auftragsNummer);
    }

    @Override
    public AuftragDTO findAuftragByNummer(int auftragsNummer) {
        Auftrag auftrag = repository.findAuftragByNummer(auftragsNummer);
        if (auftrag != null) {
            return auftrag.toDTO();
        }
        return null;
    }

    @Override
    public AuftragDTO ueberfuehreAngebotInAuftrag(AngebotDTO angebotDTO) {
        Angebot angebot = new Angebot();
        angebot.fromDTO(angebotDTO);
        return auftragsVerwaltung.ueberfuehreAngebotInAuftrag(angebot).toDTO();
    }

    @Override
    public AngebotDTO createAngebot(Datum gueltigAb, Datum gueltigBis, Betrag preis, int kundenNummer, int bauteilNummer) {
        return repository.createAngebot(gueltigAb, gueltigBis, preis, kundenNummer, bauteilNummer).toDTO();
    }

    @Override
    public AngebotDTO findAngebotByNummer(int angebotNummer) {
        return repository.findAngebotByNummer(angebotNummer).toDTO();
    }

    @Override
    public List<AngebotDTO> findAngeboteByKundenNummer(int kundenNummer) {
        List<AngebotDTO> result = new ArrayList<AngebotDTO>();
        for (Angebot angebot : repository.findAngeboteByKundenNummer(kundenNummer)) {
            result.add(angebot.toDTO());
        }
        return result;
    }

}

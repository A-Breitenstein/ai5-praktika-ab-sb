package hawmps.komponenten.auftraege.access;

import hawmps.adts.fachliche.Datum;
import hawmps.adts.fachliche.Nummer;
import hawmps.komponenten.auftraege.IAuftragsKomponente;
import hawmps.komponenten.auftraege.data_access.Auftrag;
import hawmps.komponenten.auftraege.data_access.Repository;

import javax.persistence.EntityManager;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 03.11.13
 * Time: 15:35
 */
public class AuftragsKomponente implements IAuftragsKomponente {
    private Repository repository;
    private EntityManager entityManager;

    @Override
    public Auftrag createAuftrag(Nummer nummer, boolean istAbgeschlossen, Datum beauftragtAm, Nummer fertigungsAuftragsNummer, Nummer angebotsNummer, Nummer rechnungsNummer, Nummer lieferNummer) {
        return repository.createAuftrag(nummer, istAbgeschlossen, beauftragtAm, fertigungsAuftragsNummer, angebotsNummer, rechnungsNummer, lieferNummer);
    }

    @Override
    public void updateAuftrag(Auftrag auftrag) {
        repository.updateAuftrag(auftrag);
    }

    @Override
    public void deleteAuftragByNummer(Nummer auftragsNummer) {
        repository.deleteAuftragByNummer(auftragsNummer);
    }

    @Override
    public Auftrag findAuftragByNummer(Nummer auftragsNummer) {
        return repository.findAuftragByNummer(auftragsNummer);
    }
}

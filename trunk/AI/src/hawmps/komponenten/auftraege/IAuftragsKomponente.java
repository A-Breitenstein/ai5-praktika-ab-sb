package hawmps.komponenten.auftraege;

import hawmps.adts.fachliche.Datum;
import hawmps.adts.fachliche.Nummer;
import hawmps.komponenten.auftraege.data_access.Auftrag;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 03.11.13
 * Time: 14:17
 */
public interface IAuftragsKomponente {

    Auftrag createAuftrag( boolean istAbgeschlossen, Datum beauftragtAm, Nummer fertigungsAuftragsNummer, Nummer angebotsNummer, Nummer rechnungsNummer, Nummer lieferNummer);
    void updateAuftrag(Auftrag auftrag);
    void deleteAuftragByNummer(Nummer auftragsNummer);
    Auftrag findAuftragByNummer(Nummer auftragsNummer);


}

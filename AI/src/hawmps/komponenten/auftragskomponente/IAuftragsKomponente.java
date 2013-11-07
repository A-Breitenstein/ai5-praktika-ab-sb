package hawmps.komponenten.auftragskomponente;

import hawmps.adts.fachliche.Datum;
import hawmps.komponenten.auftragskomponente.data_access.Auftrag;
import hawmps.komponenten.auftragskomponente.data_access.FertigungsAuftrag;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 03.11.13
 * Time: 14:17
 */
public interface IAuftragsKomponente {

    Auftrag createAuftrag(boolean istAbgeschlossen, Datum beauftragtAm, List<FertigungsAuftrag> zugehoerigeFertigungsAuftrage, int angebotsNummer, int rechnungsNummer, int lieferNummer);
    void updateAuftrag(Auftrag auftrag);
    void deleteAuftragByNummer(int auftragsNummer);
    Auftrag findAuftragByNummer(int auftragsNummer);

    Auftrag ueberfuehreAngebotInAuftrag(int bauteilNummer);


}

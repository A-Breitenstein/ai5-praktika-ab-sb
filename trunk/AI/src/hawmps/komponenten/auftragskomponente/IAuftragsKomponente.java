package hawmps.komponenten.auftragskomponente;

import hawmps.adts.fachliche.Betrag;
import hawmps.adts.fachliche.Datum;
import hawmps.komponenten.auftragskomponente.data_access.*;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 03.11.13
 * Time: 14:17
 */
public interface IAuftragsKomponente {

    AuftragDTO createAuftrag(boolean istAbgeschlossen, Datum beauftragtAm, List<FertigungsAuftrag> zugehoerigeFertigungsAuftrage, int angebotsNummer, int rechnungsNummer, int lieferNummer);
    void updateAuftrag(AuftragDTO auftrag);
    void deleteAuftragByNummer(int auftragsNummer);
    AuftragDTO findAuftragByNummer(int auftragsNummer);

    AuftragDTO ueberfuehreAngebotInAuftrag(AngebotDTO angebotDTO);

    AngebotDTO createAngebot(Datum gueltigAb, Datum gueltigBis, Betrag preis, int kundenNummer, int bauteilNummer);

    AngebotDTO findAngebotByNummer(int angebotNummer);

    List<AngebotDTO> findAngeboteByKundenNummer(int kundenNummer);


}

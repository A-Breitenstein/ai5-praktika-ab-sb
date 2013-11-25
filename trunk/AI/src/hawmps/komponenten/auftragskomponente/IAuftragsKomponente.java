package hawmps.komponenten.auftragskomponente;

import hawmps.adts.fachliche.Betrag;
import hawmps.adts.fachliche.Datum;
import hawmps.komponenten.auftragskomponente.data_access.*;

import java.rmi.RemoteException;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 03.11.13
 * Time: 14:17
 */
public interface IAuftragsKomponente {

    AuftragDTO createAuftrag(boolean istAbgeschlossen, Datum beauftragtAm, List<FertigungsAuftrag> zugehoerigeFertigungsAuftrage, int angebotsNummer, int rechnungsNummer, int lieferNummer) throws RemoteException;
    void updateAuftrag(AuftragDTO auftrag) throws RemoteException;
    void deleteAuftragByNummer(int auftragsNummer) throws RemoteException;
    AuftragDTO findAuftragByNummer(int auftragsNummer) throws RemoteException;

    AuftragDTO ueberfuehreAngebotInAuftrag(AngebotDTO angebotDTO) throws RemoteException;

    AngebotDTO createAngebot(Datum gueltigAb, Datum gueltigBis, Betrag preis, int kundenNummer, int bauteilNummer) throws RemoteException;

    AngebotDTO findAngebotByNummer(int angebotNummer) throws RemoteException;

    List<AngebotDTO> findAngeboteByKundenNummer(int kundenNummer) throws RemoteException;


}

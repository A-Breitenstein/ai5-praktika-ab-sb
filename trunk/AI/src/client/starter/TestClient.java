package client.starter;

import hawmps.adts.fachliche.Adresse;
import hawmps.adts.fachliche.Betrag;
import hawmps.adts.fachliche.Datum;
import hawmps.adts.fachliche.Name;
import hawmps.komponenten.auftragskomponente.data_access.AngebotDTO;
import hawmps.komponenten.auftragskomponente.data_access.AuftragDTO;
import hawmps.komponenten.bauteilkomponente.data_access.BauteilDTO;
import hawmps.komponenten.kundenkomponente.data_access.KundeDTO;
import monitor.dispatcher.IDispatcher;

import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: timey
 * Date: 26.11.13
 * Time: 19:36
 * To change this template use File | Settings | File Templates.
 */
public class TestClient {
    public static IDispatcher getDispatcher() throws RemoteException, NotBoundException {
        Registry dispatcherRegistry = LocateRegistry.getRegistry(Config.REGISTRY_HOST, Config.REGISTRY_PORT);
        IDispatcher remoteDispatcher = (IDispatcher)dispatcherRegistry.lookup(Config.DISPATCHER_NAME);
        return remoteDispatcher;
    }
    public static void main(String[] args) throws RemoteException, NotBoundException {
         IDispatcher remoteDispatcher = getDispatcher();
        //TODO toString fuer alle DTOs fuer eine bessere ausgabe waehrend der praesentation
        //Bauteil zum testen erstellen
        Name name = Name.create("Tisch");
        remoteDispatcher.getRemoteServerInstance().createTestBauteil(name);

        //Kunde ruft an und wird im System gespeichert
        KundeDTO kunde = remoteDispatcher.getRemoteServerInstance().createKunde(Name.create("Max"), Name.create("Mustermann"), Adresse.create("Musterstraße", "Musterort", "22222"));
        System.out.println(kunde);

        //Vom Kunden gewuenschtes Bauteil suchen und erstes nehmen
        BauteilDTO tisch = remoteDispatcher.getRemoteServerInstance().findBauteilByName(Name.create("Tisch")).get(0);
        System.out.println(tisch);

        //Angebot erstellen
        AngebotDTO angebot = remoteDispatcher.getRemoteServerInstance().createAngebot(Datum.create("Dezember"), Datum.create("Januar"), Betrag.create(99), kunde.getNummer(), tisch.getNummer());
        System.out.println(angebot);

        //Kunde ruft wieder an, kunde finden und ersten mit dem nachnamen auswaehlen
        List<KundeDTO> kList = remoteDispatcher.getRemoteServerInstance().findByNachname(Name.create("Mustermann"));
        KundeDTO gleicherKunde = kList.get(kList.size()-1);
        System.out.println(gleicherKunde);

        //Angebot des kunden wiederfinden und erstes nehmen
        AngebotDTO gleichesAngebot = remoteDispatcher.getRemoteServerInstance().findAngeboteByKundenNummer(gleicherKunde.getNummer()).get(0);

        //Kunde nimmt Angebot an und Angebot wird in einen Auftrag ueberfuehrt
        AuftragDTO auftrag = remoteDispatcher.getRemoteServerInstance().ueberfuehreAngebotInAuftrag(angebot);
        System.out.println(auftrag);
    }
}

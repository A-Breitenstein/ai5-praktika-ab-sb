package hawmps.starter;

import hawmps.adts.fachliche.Adresse;
import hawmps.adts.fachliche.Name;
import hawmps.dispatcher.Dispatcher;
import hawmps.dispatcher.Monitor;
import hawmps.komponenten.kundenkomponente.data_access.KundeDTO;
import hawmps.komponenten.server.MpsServer;

import java.rmi.RemoteException;

/**
 * Created with IntelliJ IDEA.
 * User: timey
 * Date: 25.11.13
 * Time: 16:50
 * To change this template use File | Settings | File Templates.
 */
public class ClientStarter {
   public static void main(String[] args) throws RemoteException, InterruptedException {
       Dispatcher dispatcher = new Dispatcher();
       Monitor monitor = Monitor.create(dispatcher);

       MpsServer server1 = MpsServer.create("s1");
       MpsServer server2 = MpsServer.create("s2");

       //TODO Ohne Speziellen Logger kann man den Inhalt der Values nicht sehen, Hibernate verbirgt das anscheinend
       //TOFU mit einem Logger auf einem Applicationserver würde man es z.B. sehen können!!!!
       //TODO entfernter methodenaufruf funktioniert aber alle parameter sind anscheinend leer
       /*Hibernate:
       insert
               into
       Kunde
               (nummer, adresse, nachname, vorname)
       values
               (null, ?, ?, ?)*/
       //damit die mps server s1 und s2 eingetragen sind
       Thread.sleep(1500);
       KundeDTO kunde = dispatcher.getRemoteServerInstance().createKunde(Name.create("Max"), Name.create("Mustermann"), Adresse.create("Musterstraße", "Musterort", "22222"));
       System.out.println(kunde);
   }
}

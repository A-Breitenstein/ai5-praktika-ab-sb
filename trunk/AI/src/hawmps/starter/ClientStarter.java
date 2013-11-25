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
   public static void main(String[] args) throws RemoteException {
       Dispatcher dispatcher = new Dispatcher();
       Monitor monitor = Monitor.create(dispatcher);

       MpsServer server1 = MpsServer.create("s1");
       MpsServer server2 = MpsServer.create("s2");

       KundeDTO kunde = dispatcher.getRemoteServerInstance().createKunde(Name.create("Max"), Name.create("Mustermann"), Adresse.create("Musterstraße", "Musterort", "22222"));
       System.out.println(kunde);
   }
}

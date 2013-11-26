package client.starter;

import hawmps.adts.fachliche.Adresse;
import hawmps.adts.fachliche.Name;
import hawmps.komponenten.kundenkomponente.data_access.KundeDTO;
import monitor.dispatcher.IDispatcher;

import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;

/**
 * Created with IntelliJ IDEA.
 * User: timey
 * Date: 26.11.13
 * Time: 19:36
 * To change this template use File | Settings | File Templates.
 */
public class TestClient {
    public static void main(String[] args) throws RemoteException, NotBoundException {
        Registry dispatcherRegistry = LocateRegistry.getRegistry(Config.REGISTRY_HOST, Config.REGISTRY_PORT);
        IDispatcher remoteDispacher = (IDispatcher)dispatcherRegistry.lookup(Config.DISPATCHER_NAME);
        KundeDTO kunde = remoteDispacher.getRemoteServerInstance().createKunde(Name.create("Max"), Name.create("Mustermann"), Adresse.create("Musterstraße", "Musterort", "22222"));
        System.out.println(kunde);
    }
}

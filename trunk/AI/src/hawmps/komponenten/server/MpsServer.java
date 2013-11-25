package hawmps.komponenten.server;

import aufgabe1.persistence.PersistenceUtilsA1;
import hawmps.fassade.HawMps;
import hawmps.starter.Config;

import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;

/**
 * Created with IntelliJ IDEA.
 * User: timey
 * Date: 24.11.13
 * Time: 14:33
 * To change this template use File | Settings | File Templates.
 */
public class MpsServer extends HawMps implements IMpsServer{
    private String serverName;
    private HeartBeat heartBeat;

    private MpsServer(String serverName, HeartBeat heartBeat) {
        super(PersistenceUtilsA1.createEntityManager());
        this.serverName = serverName;
        this.heartBeat = heartBeat;
    }

    public static MpsServer create(String serverName) throws RemoteException {
        HeartBeat heartBeat = new HeartBeat(serverName);
        MpsServer mpsSever = new MpsServer(serverName, heartBeat);
        IMpsServer stub = (IMpsServer)UnicastRemoteObject.exportObject(mpsSever, 0); //Damit wird der Server in die RMI-Runtime eingetragen, praktisch die server schleife

        Registry registry = LocateRegistry.getRegistry(Config.REGISTRY_HOST, Config.REGISTRY_PORT); //holt sich die entfernte registry
        registry.rebind(serverName, stub); //treagt den MpsServer unter dem ServerName in die Registry ein

        heartBeat.start();

        return mpsSever;
    }
}

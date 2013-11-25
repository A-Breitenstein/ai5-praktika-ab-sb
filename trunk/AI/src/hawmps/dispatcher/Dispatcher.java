package hawmps.dispatcher;

import hawmps.komponenten.server.IMpsServer;

import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.util.ArrayList;

/**
 * Created with IntelliJ IDEA.
 * User: timey
 * Date: 25.11.13
 * Time: 01:07
 * To change this template use File | Settings | File Templates.
 */
public class Dispatcher {
    public Registry serverRegistry;
    public Registry hearBeatRegistry;
    public ArrayList<IMpsServer> serverList;
    public int roundRobinCounter = 0;
    public static final int SERVER_REGISTRY_PORT = Registry.REGISTRY_PORT; // standard RegistryPort
    public static final int HEARTBEAT_REGISTRY_PORT = Registry.REGISTRY_PORT + 1; // standard RegistryPort plus 1
    public static final String REGISTRY_HOST = "localhost"; //ist selbe adresse wie die des dispatchers, da der dispatcher auch die Registry bereitstellt

    public Dispatcher() throws RemoteException {
        this.serverRegistry = LocateRegistry.createRegistry(SERVER_REGISTRY_PORT);
        this.hearBeatRegistry = LocateRegistry.getRegistry(HEARTBEAT_REGISTRY_PORT);
    }

    public IMpsServer getRemoteServerInstance(){
        return getNextRemoteObject();
    }

    private synchronized IMpsServer getNextRemoteObject(){
        if(roundRobinCounter >= serverList.size()){roundRobinCounter = 0;}
        IMpsServer server = serverList.get(roundRobinCounter);
        roundRobinCounter++;
        return server;
    }

    public synchronized void alive(IMpsServer serverInstance){
        if(serverList.contains(serverInstance)){
            serverList.add(serverInstance);
        }
    }

    public synchronized void notAlive(IMpsServer serverInstance){
        if(serverList.contains(serverInstance)){
            serverList.remove(serverInstance);
        }
    }

}

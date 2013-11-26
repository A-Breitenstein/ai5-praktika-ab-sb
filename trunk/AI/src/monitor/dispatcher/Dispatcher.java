package monitor.dispatcher;

import hawmps.komponenten.server.IMpsServer;
import client.starter.Config;
import monitor.gui.MonitorGUI;

import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.ArrayList;

/**
 * Created with IntelliJ IDEA.
 * User: timey
 * Date: 25.11.13
 * Time: 01:07
 * To change this template use File | Settings | File Templates.
 */
public class Dispatcher implements IDispatcher{
    public Registry serverRegistry;
    public ArrayList<IMpsServer> serverList;
    public int roundRobinCounter = 0;
    public static Dispatcher dispatcher = null;

    public Dispatcher() throws RemoteException {
        dispatcher = this;
        this.serverRegistry = LocateRegistry.createRegistry(Config.REGISTRY_PORT);
        this.serverList = new ArrayList<IMpsServer>();
    }

    public static Dispatcher create() throws RemoteException {
        if(dispatcher == null){
            dispatcher = new Dispatcher();
            IDispatcher stub = (IDispatcher) UnicastRemoteObject.exportObject(dispatcher, 0);

            Registry dispatcherRegistry = dispatcher.serverRegistry;
            dispatcherRegistry.rebind(Config.DISPATCHER_NAME, stub); //treagt den dispatcher unter dem namen in die Registry ein
        }
        return dispatcher;
    }

    @Override
    public IMpsServer getRemoteServerInstance() throws RemoteException{
        /*try {
            System.out.println("Namen in Registry:"+ Arrays.toString(serverRegistry.list()));
        } catch (RemoteException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }*/

        return getNextRemoteObject();
    }

    private synchronized IMpsServer getNextRemoteObject(){
        IMpsServer server = null;
        try {
            server = getNextActiveServer();
            while (server == null) {
                Thread.sleep(5000);
                server = getNextActiveServer();
            }
        } catch (RemoteException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        } catch (InterruptedException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
        return server;
    }

    private IMpsServer getNextActiveServer() throws RemoteException {
       roundRobinCounter %= serverList.size();
       IMpsServer server;
       while(roundRobinCounter < serverList.size()) {
            server = serverList.get(roundRobinCounter);
            if (!server.isDeaktiviert()) {
                //TODO dirty aber geht erstmal nicht besser
                if(server.getName().equals(Config.HAWMPS1_NAME)){MonitorGUI.getInstance().erhoeheAnfragenVonMPS1();}
                if(server.getName().equals(Config.HAWMPS2_NAME)){MonitorGUI.getInstance().erhoeheAnfragenVonMPS2();}
                roundRobinCounter++;
                return server;
            }
           roundRobinCounter++;
        }
        return null;  //To change body of created methods use File | Settings | File Templates.
    }

    public synchronized void alive(IMpsServer serverInstance){
        if(!serverList.contains(serverInstance)){
            serverList.add(serverInstance);
        }
    }

    public synchronized void notAlive(IMpsServer serverInstance){
        if(serverList.contains(serverInstance)){
            serverList.remove(serverInstance);
        }
    }

    public void deaktiviereServerInstanz(String servername,boolean b) {
        for (IMpsServer server : serverList) {
            try{
                if (server.getName().equals(servername)) {
                        server.setisDeaktiviert(b);

                }
            } catch (RemoteException e) {
                e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
            }
        }
    }


}

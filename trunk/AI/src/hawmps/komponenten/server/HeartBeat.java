package hawmps.komponenten.server;

import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import hawmps.dispatcher.IMonitor;

/**
 * Created with IntelliJ IDEA.
 * User: timey
 * Date: 25.11.13
 * Time: 01:28
 * To change this template use File | Settings | File Templates.
 */
public class HeartBeat extends Thread{
    private String serverName;
    public static final int hearBeatInterval = 1000; //in Millisekunden
    public static final int HEARTBEAT_REGISTRY_PORT = Registry.REGISTRY_PORT + 1; // standard RegistryPort plus 1
    public static final String REGISTRY_HOST = "localhost"; //ist selbe adresse wie die des dispatchers

    public HeartBeat(String serverName){
      this.serverName = serverName;
    }


   public void run(){
       while(!interrupted()){
           try {
               Registry monitorRegistry = LocateRegistry.getRegistry(REGISTRY_HOST, HEARTBEAT_REGISTRY_PORT);
               IMonitor monitor = (IMonitor)monitorRegistry.lookup(monitorRegistry.list()[0]);
               monitor.alive(serverName);
               sleep(hearBeatInterval);
           } catch (RemoteException e) {
               e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
           } catch (InterruptedException e) {
               e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
           } catch (NotBoundException e) {
               e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
           }
       }
   }
}

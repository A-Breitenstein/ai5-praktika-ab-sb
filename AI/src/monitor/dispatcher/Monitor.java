package monitor.dispatcher;

import hawmps.komponenten.server.IMpsServer;
import client.starter.Config;

import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.HashMap;
import java.util.Timer;
import java.util.TimerTask;

/**
 * Created with IntelliJ IDEA.
 * User: timey
 * Date: 25.11.13
 * Time: 01:28
 * To change this template use File | Settings | File Templates.
 */
public class Monitor implements IMonitor{
    public transient Dispatcher dispatcher;
    public transient HashMap<String, Timer> aliveTimer;
    public transient static final int timeOut = 10000;

    private Monitor(Dispatcher dispatcher){
        this.dispatcher = dispatcher;
        this.aliveTimer = new HashMap<String, Timer>();
    }

    public static Monitor create(Dispatcher dispatcher) throws RemoteException {
        Monitor monitor = new Monitor(dispatcher);
        IMonitor stub = (IMonitor)UnicastRemoteObject.exportObject(monitor, 0);

        Registry heartBeatRegistry = dispatcher.serverRegistry;
        heartBeatRegistry.rebind(Config.MONITOR_NAME, stub); //treagt den Monitor unter dem namen in die Registry ein

        return monitor;
    }

    @Override
    public void alive(String serverName) {
        //System.out.println("HeartBeat empfangen");
        if (aliveTimer.containsKey(serverName)){
            aliveTimer.get(serverName).cancel();
            aliveTimer.remove(serverName);
        }
        aliveTimer.put(serverName, startTimer(serverName));
        try {
            dispatcher.alive((IMpsServer)dispatcher.serverRegistry.lookup(serverName));
        } catch (RemoteException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        } catch (NotBoundException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
    }

    public void notAlive(String serverName) {
        aliveTimer.remove(serverName);
        try {
            IMpsServer server = (IMpsServer)dispatcher.serverRegistry.lookup(serverName);
            dispatcher.notAlive(server);
        } catch (RemoteException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        } catch (NotBoundException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
    }

    private Timer startTimer(final String serverName)  {
        final Monitor monitor = this;
        Timer timer = new Timer();
        timer.schedule(new TimerTask() {
            @Override
            public void run() {
               monitor.notAlive(serverName);
            }
        },timeOut);
        return timer;
    }
}
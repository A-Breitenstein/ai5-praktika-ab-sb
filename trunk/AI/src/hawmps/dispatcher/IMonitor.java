package hawmps.dispatcher;

import java.rmi.NotBoundException;
import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * Created with IntelliJ IDEA.
 * User: timey
 * Date: 25.11.13
 * Time: 02:18
 * To change this template use File | Settings | File Templates.
 */
public interface IMonitor extends Remote {
    public void alive(String serverName) throws RemoteException, NotBoundException;
}

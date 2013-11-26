package monitor.dispatcher;

import hawmps.komponenten.server.IMpsServer;

import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * Created with IntelliJ IDEA.
 * User: timey
 * Date: 26.11.13
 * Time: 18:43
 * To change this template use File | Settings | File Templates.
 */
public interface IDispatcher extends Remote {
    public IMpsServer getRemoteServerInstance() throws RemoteException;

}

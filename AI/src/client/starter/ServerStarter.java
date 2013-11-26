package client.starter;

import hawmps.komponenten.server.MpsServer;

import java.rmi.RemoteException;

/**
 * Created with IntelliJ IDEA.
 * User: timey
 * Date: 26.11.13
 * Time: 19:41
 * To change this template use File | Settings | File Templates.
 */
public class ServerStarter {
    public static void main(String[] args) throws RemoteException {
        MpsServer server1 = MpsServer.create(Config.HAWMPS1_NAME);
        MpsServer server2 = MpsServer.create(Config.HAWMPS2_NAME);
    }
}

package mware_lib;

import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;

/**
 * core of the middleware:
 * Maintains a Reference to the NameService
 * Singleton
 */
public class ObjectBroker {
    InetAddress inetAddress;
    int port;
    Socket clientSocket;

    public ObjectBroker(InetAddress inetAddress, int port) {
        this.inetAddress = inetAddress;
        this.port = port;
    }

    /**
     * @return an Implementation for a local NameService
     */
    public NameService getNameService() {return null;}
    /**
     * shuts down the process, the OjectBroker is running in
     * terminates process
     */
    public void shutdown() {  }
    /**
     * Initializes the ObjectBroker / creates the local NameService
     * @param serviceName
     * hostname or IP of Nameservice
     * @param port
     * port NameService is listening at
     * @return an ObjectBroker Interface to Nameservice
     */
    public static ObjectBroker init(String serviceName, int port) {
              ObjectBroker objectBroker = null;
        try {
            objectBroker = new ObjectBroker(InetAddress.getByName(serviceName), port);
        } catch (UnknownHostException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
        return objectBroker;
    }
}
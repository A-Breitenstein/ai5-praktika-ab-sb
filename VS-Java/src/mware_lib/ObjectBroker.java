package mware_lib;

import name_service.NameServiceImpl;
import name_service.NameServiceMessage;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
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
    NameService nameService;
    ObjectOutputStream objOS;
    ObjectInputStream objIS;

    public ObjectBroker(InetAddress inetAddress, int port) {
        this.inetAddress = inetAddress;
        this.port = port;

        try {
            clientSocket = new Socket(inetAddress, port);
        } catch (IOException e) {
            e.printStackTrace();
            System.out.println("ObjectBroker: NameService not found");
        }
    }

    /**
     * @return an Implementation for a local NameService
     */
    public NameService getNameService() {
        if (nameService == null) {
            try {
                objOS = new ObjectOutputStream(clientSocket.getOutputStream());
                objIS = new ObjectInputStream(clientSocket.getInputStream());
                nameService = new NameServiceImpl(objOS, objIS);
            } catch (IOException e) {
                e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
            }
        }
        return nameService;

    }
    /**
     * shuts down the process, the OjectBroker is running in
     * terminates process
     */
    public void shutDown() {
        try {
            objOS.writeObject(new NameServiceMessage(NameServiceMessage.Operations.CLOSE_CON,null,0,null));
            objIS.close();
            objOS.close();
            clientSocket.close();
            //TODO ObjectServer shutDown auch machen?
        } catch (IOException e) {
            e.printStackTrace();
            System.out.println("ObjectBroker: cant close NameService connection");
        }
    }
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
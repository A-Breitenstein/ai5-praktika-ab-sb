package name_service;

import mware_lib.Config;
import mware_lib.NameService;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;

/**
 * Created with IntelliJ IDEA.
 * User: abg667
 * Date: 18.11.13
 * Time: 13:04
 * To change this template use File | Settings | File Templates.
 */
public class NameServiceImpl extends NameService {
    Socket clientSocket;

    public NameServiceImpl(Socket clientSocket) {
        this.clientSocket = clientSocket;
    }

    @Override
    public void rebind(Object servant, String name) {
        try {
            ObjectOutputStream objOS = new ObjectOutputStream(clientSocket.getOutputStream());
            objOS.writeObject(new NameServiceMessage(NameServiceMessage.Operations.REBIND, InetAddress.getLocalHost(), Config.OBJECT_SERVER_PORT,name));
            objOS.close();
        } catch (IOException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
    }

    @Override
    public Object resolve(String name) {
        NameServiceMessage nameServiceMessage = null;
        try {
            ObjectOutputStream objOS = new ObjectOutputStream(clientSocket.getOutputStream());
            ObjectInputStream objIS = new ObjectInputStream(clientSocket.getInputStream());

            objOS.writeObject(new NameServiceMessage(NameServiceMessage.Operations.RESOLVE,null,0,name));
            nameServiceMessage = (NameServiceMessage) objIS.readObject();

            objIS.close();
            objOS.close();

        } catch (IOException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        } catch (ClassNotFoundException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
        return (Object)nameServiceMessage;
    }

    public static void main(String[] args) {

        try {
            NameService nameServiceClient = new NameServiceImpl(new Socket(InetAddress.getLocalHost(), Config.NAME_SERVER_PORT));
            NameService nameServiceServer = new NameServiceImpl(new Socket(InetAddress.getLocalHost(), Config.NAME_SERVER_PORT));
            System.out.println("rebind called");
            nameServiceServer.rebind(null,args[0]);
            System.out.println(nameServiceClient.resolve(args[0]));
        } catch (IOException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
    }
}

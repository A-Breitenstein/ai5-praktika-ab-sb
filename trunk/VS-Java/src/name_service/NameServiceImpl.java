package name_service;

import mware_lib.Config;
import mware_lib.NameService;

import java.io.IOException;
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
        } catch (IOException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
    }

    @Override
    public Object resolve(String name) {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }
}

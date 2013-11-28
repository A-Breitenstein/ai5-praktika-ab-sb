package name_service;

import bank_access.AccountImplServant;
import bank_access.AccountImplStub;
import bank_access.OverdraftException;
import mware_lib.*;

import java.io.*;
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
    ObjectOutputStream objOS;
    ObjectInputStream objIS;


    public NameServiceImpl(ObjectOutputStream outputStream, ObjectInputStream inputStream) {
            objOS = outputStream;
            objIS = inputStream;
    }

    @Override
    public void rebind(Object servant, String name) {
        try {
            objOS.writeObject(new NameServiceMessage(NameServiceMessage.Operations.REBIND, InetAddress.getLocalHost(), Config.OBJECT_SERVER_PORT, name));

            ObjectServer.getInstance().rebind((Servant) servant, name);
        } catch (IOException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
    }

    @Override
    public Object resolve(String name) {
        NameServiceMessage nameServiceMessage = null;
        try {

            objOS.writeObject(new NameServiceMessage(NameServiceMessage.Operations.RESOLVE,null,0,name));
            nameServiceMessage = (NameServiceMessage) objIS.readObject();

        } catch (IOException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        } catch (ClassNotFoundException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
        return (Object)nameServiceMessage;
    }

    public static void main(String[] args) {

        try {
            if (args[0].equals("client")) {
                ObjectBroker objectBroker = ObjectBroker.init("localhost", Config.NAME_SERVER_PORT);
                NameService nameServiceClient = objectBroker.getNameService();

                NameServiceMessage msg = (NameServiceMessage) nameServiceClient.resolve(args[1]);
                System.out.println("resolve: " + msg);
                AccountImplStub stub = new AccountImplStub(msg);
                stub.transfer(100.0);

                objectBroker.shutDown();

            }else if (args[0].equals("server")) {
                System.out.println("rebind called");
                ObjectBroker objectBroker = ObjectBroker.init("localhost",Config.NAME_SERVER_PORT);
                NameService nameServiceServer = objectBroker.getNameService();

                nameServiceServer.rebind(new AccountImplServant(100.0), args[1]);
                nameServiceServer.rebind(new AccountImplServant(150.0), args[1]+"ab");

                objectBroker.shutDown();

            }else System.out.println("fail...");

        }  catch (OverdraftException e) {
            e.printStackTrace();
        }
    }
}

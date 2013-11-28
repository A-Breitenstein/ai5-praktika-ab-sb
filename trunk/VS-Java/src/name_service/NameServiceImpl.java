package name_service;

import bank_access.AccountImplServant;
import bank_access.AccountImplStub;
import bank_access.OverdraftException;
import mware_lib.*;

import java.io.*;
import java.net.InetAddress;
import java.net.Socket;
import java.util.Random;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

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
            ObjectServer.getInstance().rebind(servant, name);
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
            objOS.writeObject(new NameServiceMessage(NameServiceMessage.Operations.CLOSE_CON,null,0,null));

        } catch (IOException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        } catch (ClassNotFoundException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
        return (Object)nameServiceMessage;
    }

    public static void main(final String[] args) {

            if (args[0].equals("client")) {
                ExecutorService threadPool = Executors.newCachedThreadPool();
                for (int i = 0; i < 25; i++) {

                    threadPool.execute(new Runnable() {
                        @Override
                        public void run() {
                            ObjectBroker objectBroker = ObjectBroker.init("localhost", Config.NAME_SERVER_PORT);
                            NameService nameServiceClient = objectBroker.getNameService();

                            NameServiceMessage msg = (NameServiceMessage) nameServiceClient.resolve(args[1]);
                            System.out.println("resolve: " + msg);
                            AccountImplStub stub = new AccountImplStub(msg);

                            Random random = new Random();
                            System.out.println("CurrentBalance before 10 transfers: "+stub.getBalance());
                            for (int i = 0; i < 50; i++) {
                                try {
                                    stub.transfer(random.nextDouble() * 100);
                                } catch (OverdraftException e) {
                                    e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                                }
                                System.out.println("CurrentBalance transfer "+(i+1)+": "+stub.getBalance());
                            }
                            System.out.println("CurrentBalance after 10 transfers: "+stub.getBalance());


                            objectBroker.shutDown();
                        }
                    });
                }


            }else if (args[0].equals("server")) {
                System.out.println("rebind called");
                ObjectBroker objectBroker = ObjectBroker.init("localhost",Config.NAME_SERVER_PORT);
                NameService nameServiceServer = objectBroker.getNameService();

                nameServiceServer.rebind(new AccountImplServant(100.0), args[1]);
                nameServiceServer.rebind(new AccountImplServant(150.0), args[1]+"ab");

                objectBroker.shutDown();

            }else System.out.println("fail...");

    }
}

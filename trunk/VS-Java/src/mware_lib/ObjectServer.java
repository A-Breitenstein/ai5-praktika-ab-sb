package mware_lib;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 27.11.13
 * Time: 16:21
 * To change this template use File | Settings | File Templates.
 */
public class ObjectServer {
    ExecutorService threadPool;
    ServerSocket serverSocket;
    int port = Config.OBJECT_SERVER_PORT;
    int max_connections = 10;
    int current_connections = 0;
    Thread dispatcherThread;


    Map<String,Servant> objectDirectory = new HashMap<String, Servant>();
    private static ObjectServer instance;

    public ObjectServer() {
        try {
            serverSocket = new ServerSocket(port);
            serverSocket.setSoTimeout(7000);
            threadPool = Executors.newCachedThreadPool();
            System.out.println("ObjectServer stated on "+ InetAddress.getLocalHost() +":"+port);

            new Thread(new Runnable() {
                @Override
                public void run() {
                    dispatcherThread = Thread.currentThread();
                    looper();

                }
            }).start();
            Thread.sleep(100);
        } catch (IOException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

    }



    public void looper() {

        while (true){
            try {
                final Socket clientSocket;
                if (current_connections < max_connections) {
                    clientSocket= serverSocket.accept();

                    threadPool.execute(new Runnable() {
                        @Override
                        public void run() {
                            try {
                                ObjectInputStream objIS = new ObjectInputStream(clientSocket.getInputStream());
                                ObjectOutputStream objOS = new ObjectOutputStream(clientSocket.getOutputStream());

                                ObjectServerMessage serviceMessage = (ObjectServerMessage) objIS.readObject();
                                System.out.println("ObjectServer: received: "+serviceMessage);
                                Servant servant = objectDirectory.get(serviceMessage.getObjectID());

                                if (servant == null) {
                                    serviceMessage.setMsg(ObjectServerMessage.Msg.OBJECT_NOT_FOUND);
                                    objOS.writeObject(serviceMessage);
                                }else {
                                    Skeleton skeleton = servant.createSkeleton(clientSocket, serviceMessage);
                                    System.out.println("ObjectServer: "+servant.getReferences()+" references on"+servant.getClass().toString());
                                    serviceMessage.setMsg(ObjectServerMessage.Msg.OBJECT_FOUND);

                                    Object returnObject = skeleton.callFunction(serviceMessage);

                                    serviceMessage.setReturnVal(returnObject);
                                    System.out.println("ObjectServer: send: "+serviceMessage );
                                    objOS.writeObject(serviceMessage);
                                    servant.removeSkeleton(skeleton);
                                }

                                objIS.close();
                                objOS.close();
                                clientSocket.close();
                            } catch (IOException e) {
                                e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                            } catch (ClassNotFoundException e) {
                                e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                            }
                            clientLeft();
                        }
                    });
                    current_connections++;
                    printCurrentConnections(current_connections);
                }else{
                    try {
                        Thread.sleep(10000000);
                    } catch (InterruptedException e) {
                    }
                }
            } catch (IOException e) {
//                e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
            }
        }
    }

    private void printCurrentConnections(int current_connections) {
        System.out.println("ObjectServer: Aktuelle Verbindungen: " +current_connections);
    }

    public synchronized void clientLeft() {
        if (current_connections < max_connections) {
            current_connections--;
            printCurrentConnections(current_connections);
        }
        else{
            current_connections--;
            dispatcherThread.interrupt();
        }
    }

    public static ObjectServer getInstance() {
        if (instance == null) {
            instance = new ObjectServer();
        }
        return instance;
    }

    public static void main(String[] args) {
        new ObjectServer();
    }


    public void rebind(Servant servant, String objectID) {
        objectDirectory.put(objectID, servant);
        System.out.println("ObjectServer: rebind: objectID = "+objectID+", "+servant);
    }
}

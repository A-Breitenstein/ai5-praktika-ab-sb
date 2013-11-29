package mware_lib.object_server;


import mware_lib.Config;
import mware_lib.skeleton.Skeleton;
import mware_lib.skeleton.SkeletonFactory;
import mware_lib.skeleton.SkeletonFactoryImpl;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.*;
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
    int port;
    int max_connections = 10;
    int current_connections = 0;
    Thread dispatcherThread;
    
    public static final boolean DEBUG =false;

    Map<String,SkeletonFactory> objectDirectory = new HashMap<String, SkeletonFactory>();
    private static ObjectServer instance;

    public ObjectServer() {
        try {
            port = findFreePort(Config.OBJECT_SERVER_PORT);
            serverSocket.setSoTimeout(7000);
            threadPool = Executors.newCachedThreadPool();
            System.out.println("ObjectServer stated on " + InetAddress.getLocalHost() + ":" + port);

            new Thread(new Runnable() {
                @Override
                public void run() {
                    dispatcherThread = Thread.currentThread();
                    looper();

                }
            }).start();
            Thread.sleep(100);
        } catch (InterruptedException e) {
            e.printStackTrace();
        } catch (UnknownHostException e) {
            e.printStackTrace();
        } catch (SocketException e) {
            e.printStackTrace();
        }

    }

    private int findFreePort(int port) {
        int portMax = port + Config.OBJECT_SERVER_PORT_RANGE;
        for (int i = port; i < portMax; i++) {
            try {
                serverSocket = new ServerSocket(i);
                return i;
            } catch (IOException e) {

            }
        }
        throw new UnknownError("ObjectServer: cant find free port, adjust OBJECT_SERVER_PORT_RANGE in Config");
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
                                if(Config.DEBUG) System.out.println("ObjectServer: received: "+serviceMessage);
                                SkeletonFactory skeletonFactory = objectDirectory.get(serviceMessage.getObjectID());

                                if (skeletonFactory == null) {
                                    serviceMessage.setMsg(ObjectServerMessage.Msg.OBJECT_NOT_FOUND);
                                    objOS.writeObject(serviceMessage);
                                }else {
                                    Skeleton skeleton = skeletonFactory.createSkeleton(clientSocket, serviceMessage);
                                    if(Config.DEBUG) System.out.println("ObjectServer: " + skeletonFactory.getReferences() + " references on" + skeleton.getClass().toString());
                                    serviceMessage.setMsg(ObjectServerMessage.Msg.OBJECT_FOUND);

                                    Object returnObject = skeleton.callFunction(serviceMessage);

                                    serviceMessage.setReturnVal(returnObject);
                                    if(Config.DEBUG) System.out.println("ObjectServer: send: " + serviceMessage);
                                    objOS.writeObject(serviceMessage);
                                    skeletonFactory.removeSkeleton(skeleton);
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
        if(Config.DEBUG) System.out.println("ObjectServer: Aktuelle Verbindungen: " +current_connections);
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

    public void rebind(Object servant, String objectID) {
        objectDirectory.put(objectID,new SkeletonFactoryImpl(servant));
        if(Config.DEBUG) System.out.println("ObjectServer: rebind: objectID = " + objectID + ", " + servant);
        for (String s : objectDirectory.keySet()) {
            if(Config.DEBUG) System.out.println("ObjectServer: binded objects: "+s+" => "+objectDirectory.get(s));
        }
    }

    public int getPort() {
        return port;
    }
}

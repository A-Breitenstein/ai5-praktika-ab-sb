package mware_lib;

import bank_access.AccountImplServant;
import bank_access.AccountSkeletonFactory;
import bank_access.ManagerImplServant;
import bank_access.ManagerSkeletonFactory;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
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

    List<SkeletonFactory> skeletonFactoryList = new ArrayList<SkeletonFactory>();

    Map<String,SkeletonFactory> objectDirectory = new HashMap<String, SkeletonFactory>();
    private static ObjectServer instance;

    public ObjectServer() {
        skeletonFactoryList.add(new AccountSkeletonFactory());
        skeletonFactoryList.add(new ManagerSkeletonFactory());
        try {
            serverSocket = new ServerSocket(port);
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
                                SkeletonFactory skeletonFactory = objectDirectory.get(serviceMessage.getObjectID());

                                if (skeletonFactory == null) {
                                    serviceMessage.setMsg(ObjectServerMessage.Msg.OBJECT_NOT_FOUND);
                                    objOS.writeObject(serviceMessage);
                                }else {
                                    Skeleton skeleton = skeletonFactory.createSkeleton(clientSocket, serviceMessage);
                                    System.out.println("ObjectServer: " + skeletonFactory.getReferences() + " references on" + skeletonFactory.getClass().toString());
                                    serviceMessage.setMsg(ObjectServerMessage.Msg.OBJECT_FOUND);

                                    Object returnObject = skeleton.callFunction(serviceMessage);

                                    serviceMessage.setReturnVal(returnObject);
                                    System.out.println("ObjectServer: send: " + serviceMessage);
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

        //TODO:Ohne SkeletonFactoryList und dynamisch, Liste muss nicht bei neuen SkeletonFactories ergänzt werden
        //Man spart sich das Hinzufügen in die Liste, sowie das halten einer liste,
        //lediglich Methoden in der ImplBase muss vorhanden sein->über Interface lösbar
        sogehtsAuch();

        /*
        * Weitere Möglichkeit: Abstracts implementieren SkeletonFactoryInterface, (Object) Servant cast zu SkeletonFactoryInterface
        * aufruf der Methode skeletonFactory() -> super Delegation -> Abstract Class erstellt Kontextbezogenen SkeletonFactory als Rückgabewert
        */
    }

    /**
     * Java Reflection Möglichkeit, anderer Weg siehe oben
     */
    private static void sogehtsAuch() {
        Object servant;

        final String const_SkeletonFactoryMethodName = "skeletonFactory";
        // ACCOUNT IMPL SERVANT
        servant = new AccountImplServant(123.3);

        Method[] methods = servant.getClass().getSuperclass().getMethods();

        SkeletonFactory skeletonFactory = null;
        for (Method method : methods) {

            if (method.getName().equals(const_SkeletonFactoryMethodName)) {
                try {
                    skeletonFactory = (SkeletonFactory) method.invoke(servant.getClass().getSuperclass(), new Object[]{});
                    break;
                } catch (IllegalAccessException e) {
                    e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                } catch (InvocationTargetException e) {
                    e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                }
            }
        }
        System.out.println(skeletonFactory);

        //MANAGER IMPL SERVANT
        servant = new ManagerImplServant();

        methods = servant.getClass().getSuperclass().getMethods();

        skeletonFactory = null;
        for (Method method : methods) {

            if (method.getName().equals(const_SkeletonFactoryMethodName)) {
                try {
                    skeletonFactory = (SkeletonFactory) method.invoke(servant.getClass().getSuperclass(), new Object[]{});
                    break;
                } catch (IllegalAccessException e) {
                    e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                } catch (InvocationTargetException e) {
                    e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                }
            }
        }
        System.out.println(skeletonFactory);

    }


    public void rebind(Object servant, String objectID) {
        objectDirectory.put(objectID,createSkeletonFactory(servant));
        System.out.println("ObjectServer: rebind: objectID = " + objectID + ", " + servant);
    }

    private SkeletonFactory createSkeletonFactory(Object servant) {
        System.out.println(servant.getClass().getSuperclass());
        //TODO: Siehe: sogehtsauch()
        //TODO: hier muss geguckt welchen vom welchem typ der servant ist, und dann muss die passende
        //skeletonfactory ausgewählt werden und der servant auf seine superclass gecastet werden
        //damit man ihn der skeletonfactory per constructor übergeben kann

        System.out.println("ObjectServer: trying to find suiteable SkeletonFactory for: "+servant.getClass().getSuperclass());
        for (SkeletonFactory skeletonFactory : skeletonFactoryList) {
            if (skeletonFactory.getServantClass().equals(servant.getClass().getSuperclass())) {
                System.out.println("ObjectServer: found!");
                return skeletonFactory;
            }
        }




        throw new ClassCastException("ObjectServer: couldnt find suiteable skeletonFactory");
    }


}

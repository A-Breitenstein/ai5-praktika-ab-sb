package name_service;

import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * Created with IntelliJ IDEA.
 * User: abg628
 * Date: 27.11.13
 * Time: 15:32
 * To change this template use File | Settings | File Templates.
 */
public class NameServiceServer {
    ExecutorService threadPool;
    ServerSocket serverSocket;
    int port = 50000;
    int max_connections = 10;
    int current_connections = 0;
    Thread dispatcherThread;

    Map<String,NameServiceMessage> nameDirectory = new HashMap<String, NameServiceMessage>();

    public NameServiceServer() {
        dispatcherThread = Thread.currentThread();
        try {
            serverSocket = new ServerSocket(port);
            serverSocket.setSoTimeout(7000);
            threadPool = Executors.newCachedThreadPool();

            looper();
        } catch (IOException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
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

                                NameServiceMessage serviceMessage = (NameServiceMessage) objIS.readObject();
                                switch (serviceMessage.operation) {
                                    case REBIND:
                                        rebind(serviceMessage);
                                        break;
                                    case RESOLVE:
                                        objOS.writeObject(resolve(serviceMessage));
                                        break;
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
                    System.out.println(current_connections);
                }else{
                    try {
                        Thread.sleep(1000000);
                    } catch (InterruptedException e) {

                    }
                }
            } catch (IOException e) {
//                e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
            }
        }
    }

    private void rebind(NameServiceMessage serviceMessage) {
        nameDirectory.put(serviceMessage.id, serviceMessage);
    }

    private NameServiceMessage resolve(NameServiceMessage serviceMessage) {
        return nameDirectory.get(serviceMessage.id);
    }

    public synchronized void clientLeft() {
        if (current_connections < max_connections) {
            current_connections--;
            System.out.println(current_connections);
        }
        else{
            current_connections--;
            dispatcherThread.interrupt();
        }
    }

    public static void main(String[] args) {
        new NameServiceServer();
    }



}

package hawmps;

import aufgabe1.persistence.PersistenceUtilsA1;
import hawmps.adts.fachliche.Adresse;
import hawmps.adts.fachliche.Name;
import hawmps.fassade.HawMps;
import hawmps.fassade.ISystemFassade;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 24.11.13
 * Time: 19:03
 */
public class HawMpsMain {

    ExecutorService threadPool;
    ServerSocket serverSocket;
    int port = 50000;
    int max_connections = 10;
    int current_connections = 0;
    private static HawMpsMain instance;
    Thread dispatcherThread;

    public HawMpsMain() {
        instance = this;
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

    public static HawMpsMain getInstance() {
        return instance;
    }

    public void looper() {

        while (true){
            try {
                Socket clientSocket;
                if (current_connections < max_connections) {
                    clientSocket= serverSocket.accept();
                    threadPool.execute(new Runnable() {
                        @Override
                        public void run() {
                            ISystemFassade hawmps = new HawMps(PersistenceUtilsA1.createEntityManager());

                            //#TODO hier sollte jetzt irgendwas aufgerufen werden auf das mps

                            //hawmps.createKunde(Name.create("Peter"), Name.create("Lustig"), Adresse.create("", "", ""));
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
        new HawMpsMain();
    }

}

package hawmps;

import aufgabe1.persistence.PersistenceUtilsA1;
import hawmps.adts.fachliche.Adresse;
import hawmps.adts.fachliche.Name;
import hawmps.fassade.HawMps;
import hawmps.fassade.ISystemFassade;
import hawmps.komponenten.kundenkomponente.data_access.KundeDTO;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
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
        System.out.println("HawMpsMain gestartet");
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
                final Socket clientSocket;
                if (current_connections < max_connections) {
                    clientSocket= serverSocket.accept();
                    threadPool.execute(new Runnable() {
                        @Override
                        public void run() {
                            ISystemFassade hawmps = new HawMps(PersistenceUtilsA1.createEntityManager());
                            try {
                                ObjectInputStream objIS = new ObjectInputStream(clientSocket.getInputStream());
                                ObjectOutputStream objOS = new ObjectOutputStream(clientSocket.getOutputStream());
                                // eingegange message lesen
                                Message message = (Message) objIS.readObject();
                                // message auswerten und gegebenfalls returnvalue zuweisen
                                switch (message.funktion) {
                                    case createKunde:
                                        System.out.println("createKunde wurde aufgerufen");
                                        message.rueckgabewert = hawmps.createKunde((Name) message.parameter[0], (Name) message.parameter[1], (Adresse) message.parameter[2]);
                                        break;
                                    case findByNachname:
                                        message.rueckgabewert = hawmps.findByNachname((Name)message.parameter[0]);
                                        break;
                                    case deleteKundeByNummer:
                                        hawmps.deleteKundeByNummer((Integer)message.parameter[0]);
                                        break;
                                }
                                // message zurück senden
                                objOS.writeObject(message);

                                objIS.close();
                                objOS.close();

                            } catch (IOException e) {
                                e.printStackTrace();
                            } catch (ClassNotFoundException e) {
                                e.printStackTrace();
                            }

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

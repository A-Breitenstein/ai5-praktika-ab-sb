package read_write_object_example;

import hawmps.adts.fachliche.Adresse;
import hawmps.adts.fachliche.Name;
import hawmps.komponenten.kundenkomponente.data_access.KundeDTO;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.concurrent.Executors;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 24.11.13
 * Time: 19:30
 */
public class Server {

    public static void main(String[] args) {
        try {
            ServerSocket serverSocket = new ServerSocket(50000);
            Socket clientSocket = serverSocket.accept();
            ObjectInputStream objIS = new ObjectInputStream(clientSocket.getInputStream());
            ObjectOutputStream objOS = new ObjectOutputStream(clientSocket.getOutputStream());

            KundeDTO  kundeDTO  = (KundeDTO) objIS.readObject();
            System.out.println("Empfangen: "+kundeDTO);


            KundeDTO kundeDTO2 = new KundeDTO(1, Name.create("Sven"), Name.create("Lustig"), Adresse.create("", "", ""));
            objOS.writeObject(kundeDTO2);
            System.out.println("Gesendet: " + kundeDTO2);


            objIS.close();
            objOS.close();
            serverSocket.close();

        } catch (IOException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
    }

}

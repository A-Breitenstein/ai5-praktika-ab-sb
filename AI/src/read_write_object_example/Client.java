package read_write_object_example;

import hawmps.adts.fachliche.Adresse;
import hawmps.adts.fachliche.Name;
import hawmps.komponenten.kundenkomponente.data_access.KundeDTO;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.InetAddress;
import java.net.Socket;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 24.11.13
 * Time: 19:30
 */
public class Client {
    public static void main(String[] args) {
        try {
            Socket tcpSocket = new Socket(InetAddress.getLocalHost(), 50000);
            ObjectOutputStream objOS = new ObjectOutputStream(tcpSocket.getOutputStream());
            ObjectInputStream objIS = new ObjectInputStream(tcpSocket.getInputStream());
            KundeDTO kundeDTO = new KundeDTO(1, Name.create("Dieter"), Name.create("Lustig"), Adresse.create("", "", ""));


            objOS.writeObject(kundeDTO);
            System.out.println("Gesendet: " + kundeDTO);

            KundeDTO kundeDTO2 = (KundeDTO)objIS.readObject();
            System.out.println("Empfangen: "+kundeDTO2);

            objIS.close();
            objOS.close();
            tcpSocket.close();

        } catch (IOException e) {
            e.printStackTrace();
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }

    }
}

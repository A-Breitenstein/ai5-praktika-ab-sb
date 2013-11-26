package read_write_object_example;

import hawmps.adts.fachliche.Adresse;
import hawmps.adts.fachliche.Name;
import hawmps.komponenten.kundenkomponente.data_access.KundeDTO;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 24.11.13
 * Time: 20:22
 */
public class HawMpsTestClient {
    public static void main(String[] args) {
        KundeDTO maxMustermann = createKunde(Name.create("Max"), Name.create("Mustermann"), Adresse.create("Musterstraße", "Musterort", "22222"));
        List<KundeDTO> kundeDTOs = findByNachname(maxMustermann.getNachname());
        deleteKundeByNummer(maxMustermann.getNummer());
    }

    public static KundeDTO createKunde(Name Vorname,Name Nachname,Adresse adresse) {
        Object[] parameter = new Object[]{Vorname,Nachname,adresse};
        Message message = new Message(Message.Funktion.createKunde,parameter);
        return (KundeDTO)send(message).rueckgabewert;
    }
    public static List<KundeDTO> findByNachname(Name Nachname){
        Object[] parameter = new Object[]{Nachname};
        Message message = new Message(Message.Funktion.findByNachname,parameter);
        return (List<KundeDTO>)send(message).rueckgabewert;
    }
    public static void deleteKundeByNummer(int kundenNummer){
        Object[] parameter = new Object[]{new Integer(kundenNummer)};
        Message message = new Message(Message.Funktion.deleteKundeByNummer,parameter);
        send(message);
    }

    /**
     *  zum testen reichts, da immer ein neuer socket erstellt wird sollte man es so nicht unbedingt machen
     * @param message
     * @return
     */
    public static Message send(Message message) {
        Message result = null;
        try {
            Socket tcpSocket = new Socket(InetAddress.getLocalHost(), 50000);
            ObjectOutputStream objOS = new ObjectOutputStream(tcpSocket.getOutputStream());
            ObjectInputStream objIS = new ObjectInputStream(tcpSocket.getInputStream());

            objOS.writeObject(message);
            System.out.println("Gesendet: " + message);

            result = (Message)objIS.readObject();
            System.out.println("Return value: "+result.rueckgabewert);

            objIS.close();
            objOS.close();
            tcpSocket.close();

        } catch (IOException e) {
            e.printStackTrace();
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
        return  result;
    }
}

package client.starter;

import hawmps.adts.fachliche.Adresse;
import hawmps.adts.fachliche.Name;
import monitor.dispatcher.Dispatcher;
import monitor.dispatcher.Monitor;
import hawmps.komponenten.kundenkomponente.data_access.KundeDTO;
import hawmps.komponenten.server.MpsServer;
import monitor.gui.MonitorGUI;

import javax.swing.*;
import java.rmi.RemoteException;

/**
 * Created with IntelliJ IDEA.
 * User: timey
 * Date: 25.11.13
 * Time: 16:50
 * To change this template use File | Settings | File Templates.
 */
public class ClientStarter {
   public static void main(String[] args) throws RemoteException, InterruptedException {

       JFrame frame = new JFrame("MonitorGUI");
       MonitorGUI x = new MonitorGUI(frame);
       frame.setContentPane(x.monitorGUI);
       frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
       frame.pack();
       frame.setVisible(true);


       Dispatcher dispatcher = new Dispatcher();
       Monitor monitor = Monitor.create(dispatcher);

//       MpsServer server1 = MpsServer.create(Config.HAWMPS1_NAME);
       MpsServer server2 = MpsServer.create(Config.HAWMPS2_NAME);

       //TODO Ohne Speziellen Logger kann man den Inhalt der Values nicht sehen, Hibernate verbirgt das anscheinend
       //TOFU mit einem Logger auf einem Applicationserver würde man es z.B. sehen können!!!!
       //TODO entfernter methodenaufruf funktioniert aber alle parameter sind anscheinend leer
       /*Hibernate:
       insert
               into
       Kunde
               (nummer, adresse, nachname, vorname)
       values
               (null, ?, ?, ?)*/
       //damit die mps server s1 und s2 eingetragen sind
       Thread.sleep(1500);
       KundeDTO kunde = dispatcher.getRemoteServerInstance().createKunde(Name.create("Max"), Name.create("Mustermann"), Adresse.create("Musterstraße", "Musterort", "22222"));
       System.out.println(kunde);
   }
}

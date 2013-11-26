package monitor;

import hawmps.adts.fachliche.Adresse;
import hawmps.adts.fachliche.Name;
import monitor.dispatcher.Dispatcher;
import monitor.dispatcher.IDispatcher;
import monitor.dispatcher.IMonitor;
import monitor.dispatcher.Monitor;
import hawmps.komponenten.kundenkomponente.data_access.KundeDTO;
import hawmps.komponenten.server.MpsServer;
import monitor.gui.MonitorGUI;

import javax.swing.*;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.util.Arrays;

/**
 * Created with IntelliJ IDEA.
 * User: timey
 * Date: 25.11.13
 * Time: 16:50
 * To change this template use File | Settings | File Templates.
 */
public class DispatcherStarter {
   public static void main(String[] args) throws RemoteException, InterruptedException, NotBoundException {

       JFrame frame = new JFrame("MonitorGUI");
       MonitorGUI x = new MonitorGUI(frame);
       frame.setContentPane(x.monitorGUI);
       frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
       frame.pack();
       frame.setVisible(true);


       Dispatcher dispatcher = Dispatcher.create();
       Monitor monitor = Monitor.create(dispatcher);

       //damit die mps server s1 und s2 eingetragen sind
       //Thread.sleep(1500);
   }
}

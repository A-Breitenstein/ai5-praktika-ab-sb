package monitor.gui;

import client.starter.Config;
import monitor.dispatcher.Dispatcher;

import javax.sound.sampled.Line;
import javax.swing.*;
import javax.swing.border.LineBorder;
import javax.swing.border.TitledBorder;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

/**
 * Created with IntelliJ IDEA.
 * User: abg628
 * Date: 26.11.13
 * Time: 13:00
 * To change this template use File | Settings | File Templates.
 */
public class MonitorGUI {
    public JPanel monitorGUI;
    private JLabel mps1_anfragen;
    private JLabel mps2_anfragen;
    private JPanel mps2_panel;
    private JPanel mps1_panel;
    private JToggleButton mps1_btn;
    private JToggleButton mps2_btn;
    private JLabel mps1_lab_time;
    private JLabel mps2_lab_time;
    private JLabel mps1_time;
    private JLabel mps2_time;
    private  JFrame jFrame;
    private static MonitorGUI instance;

    public MonitorGUI(JFrame frame) {
        instance = this;
        jFrame = frame;
        mps1_btn.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (!mps1_btn.isSelected()) {
                    //TODO MPS1 Server in der roundrobben queue aktivieren
                    mps1_btn.setText("Aktiviert");
                    Dispatcher.dispatcher.deaktiviereServerInstanz(Config.HAWMPS1_NAME,false);
                    changeZustandMPS1(Zustand.online);

                } else {
                    //TODO MPS1 Server in der roundrobben queue deaktivieren
                    mps1_btn.setText("Deaktiviert");
                    Dispatcher.dispatcher.deaktiviereServerInstanz(Config.HAWMPS1_NAME,true);
                    changeZustandMPS1(Zustand.deaktiviert);

                }
            }
        });
        mps2_btn.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (!mps2_btn.isSelected()) {
                    //TODO MPS2 Server in der roundrobben queue aktivieren
                    mps2_btn.setText("Aktiviert");
                    Dispatcher.dispatcher.deaktiviereServerInstanz(Config.HAWMPS2_NAME,false);
                    changeZustandMPS2(Zustand.deaktiviert);
                } else {
                    //TODO MPS2 Server in der roundrobben queue deaktivieren
                    mps2_btn.setText("Deaktiviert");
                    Dispatcher.dispatcher.deaktiviereServerInstanz(Config.HAWMPS2_NAME,true);
                    changeZustandMPS2(Zustand.deaktiviert);
                }
            }
        });

    }

    public static MonitorGUI getInstance() {
        return instance;
    }

    public enum Zustand{online,offline,deaktiviert;}
    private static final TitledBorder DEAKTIVIERT = new TitledBorder(new LineBorder(Color.YELLOW,2), "deaktiviert");
    private static final TitledBorder ONLINE = new TitledBorder(new LineBorder(Color.GREEN,2), "online");
    private static final TitledBorder OFFLINE = new TitledBorder(new LineBorder(Color.RED,2), "offline");
    private void changeZustandPanel(Zustand zustand,JPanel panel) {
        TitledBorder border = null;
        switch (zustand) {
            case online:
                border = ONLINE;;
                break;
            case offline:
                border = OFFLINE;
                break;
            case deaktiviert:
                border = DEAKTIVIERT;
                break;
        }
        panel.setBorder(border);
    }
    public void changeZustandMPS1(Zustand zustand) {
        changeZustandPanel(zustand,mps1_panel);
    }
    public void changeZustandMPS2(Zustand zustand) {
        changeZustandPanel(zustand,mps2_panel);
    }
    private void erhoeheAnfragen(JLabel label) {
        label.setText(String.valueOf((Integer.valueOf(label.getText())).intValue() + 1));
    }
    public void erhoeheAnfragenVonMPS1() {
        erhoeheAnfragen(mps1_anfragen);
    }
    public void erhoeheAnfragenVonMPS2() {
        erhoeheAnfragen(mps2_anfragen);
    }

    
    
    

    public static void main(String[] args) {
        JFrame frame = new JFrame("MonitorGUI");
        MonitorGUI x = new MonitorGUI(frame);
        frame.setContentPane(x.monitorGUI);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.pack();
        frame.setVisible(true);
    }

}

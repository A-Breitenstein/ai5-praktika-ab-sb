package aufgabe6.grundgeruest;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 03.12.13
 * Time: 18:41
 */
public class Regler {
    private JSlider slider1;

    public int value;

    public JPanel getPanel1() {
        return panel1;
    }

    private JPanel panel1;
    public static void main(String[] args) {
        JFrame frame = new JFrame("Regler");
        frame.setContentPane(new Regler().panel1);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.pack();
        frame.setVisible(true);
    }
    public Regler() {

        slider1.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                value = slider1.getValue();
            }
        });
    }

}

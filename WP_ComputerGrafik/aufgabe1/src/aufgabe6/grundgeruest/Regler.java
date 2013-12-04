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
    private CG1Frame cg1Frame;

    final static int percision = 100000;

    public JPanel getPanel1() {
        return panel1;
    }

    private JPanel panel1;
    private JLabel lb_currentValue;

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
        slider1.setMinimum(0);
        slider1.setMaximum(percision);
        slider1.setValue(0);
        slider1.addChangeListener(new ChangeListener() {
        double currentValue = 0.0;
            @Override
            public void stateChanged(ChangeEvent e) {
                currentValue = ((double) slider1.getValue()) / percision;

                cg1Frame.changeTangentVector(currentValue);

                lb_currentValue.setText(String.valueOf(currentValue));
            }
        });
    }

    public void setCg1Frame(CG1Frame cg1Frame) {
        this.cg1Frame = cg1Frame;
    }
}

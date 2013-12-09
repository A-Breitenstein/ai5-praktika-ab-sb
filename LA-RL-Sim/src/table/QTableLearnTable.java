package table;

import org.encog.neural.rbf.RBFNetwork;
import org.neuroph.core.NeuralNetwork;
import org.neuroph.core.data.DataSet;
import org.neuroph.nnet.MultiLayerPerceptron;
import org.neuroph.nnet.RbfNetwork;
import org.neuroph.util.TransferFunctionType;

import java.util.Queue;
import java.util.concurrent.ArrayBlockingQueue;

/**
 * Created with IntelliJ IDEA.
 * User: abg667
 * Date: 09.12.13
 * Time: 10:43
 * To change this template use File | Settings | File Templates.
 */
public class QTableLearnTable implements QTable {
    RbfNetwork network;
    private static final int InputNeuronSize =QValueEntry.actionsBits + QValueEntry.positionBits*2;
    private static final int OutputNeuronSize = QValueEntry.qValueBits;
    Queue<QValueEntry> history = new ArrayBlockingQueue<QValueEntry>(12);

    TrainingPatterns trainingPat = new TrainingPatterns();

    private static final double tausend = 1000;

    double[][][] table;

    private int width,height, numberOfActions;

    @Override
    public void initialize(int width, int height, int numberOfActions) {
        this.width = width;
        this.height = height;
        this.numberOfActions = numberOfActions;

        table = new double[width][height][numberOfActions];
        network = (RbfNetwork)NeuralNetwork.load("NewNeuralNetwork2.nnet");

//        initNetwork();

    }

    @Override
    public void updateQValue(int xPos, int yPos, int currentAction, double qValue) {

    }

    private double[] getQValues(int xPos,int yPos) {
        double[] result = new double[numberOfActions];
        for (int action = 0; action < numberOfActions; action++) {
            result[action] = getQValue(xPos, yPos, action);
        }
        return result;
    }

    @Override
    public int getBestAction(int xPos, int yPos) {
        double[] qValues = getQValues(xPos, yPos);
        double min = qValues[0];
        int bestAction = 0;
        for (int action=1;action<qValues.length;action++)
        {
            if (min>qValues[action])
            {
                min = qValues[action];
                bestAction = action;
            }
        }
        return bestAction;
    }

    @Override
    public double getMinQValue(int xPos, int yPos) {
        double[] qValues = getQValues(xPos, yPos);
        double min = qValues[0];
        int bestAction = 0;
        for (int action=1;action<qValues.length;action++)
        {
            if (min>qValues[action])
            {
                min = qValues[action];
                bestAction = action;
            }
        }
        return min;
    }

    @Override
    public double getQValue(int xPos, int yPos, int action) {
        QValueEntry qValueEntry = new QValueEntry(xPos,yPos,action,0);
        qValueEntry.normalize(new NormalizerX1());
        //TODO input values richtig machen
        network.setInput(qValueEntry.mergedInputArray);
        network.calculate();
        final double[] output = network.getOutput();
        return binaryArrayToDouble(output);
    }

    private double binaryArrayToDouble(double[] output) {
        double value = 0;
        for (int i = output.length-1; 0 < i; i--) {
            value += (int)(output[i]+0.5)*Math.pow(2, output.length-1-i);
        }
        return value;
    }

    @Override
    public double[][][] getCompleteTable() {

        for (int x = 0; x < width; x++) {
            for (int y = 0; y < height; y++) {
                for (int action = 0; action < numberOfActions; action++) {
                    table[x][y][action] = getQValue(x, y, action);
                }
            }
        }

        return table;
    }


    public static class TrainingPatterns {

        public DataSet createTrainingPatterns(Object[] history) {
            DataSet trainingSet = new DataSet(InputNeuronSize, OutputNeuronSize);

            QValueEntry qValueEntry;
            for (int i = 0; i < history.length; i++) {
                qValueEntry = (QValueEntry)history[i];
                //TODO: trainingSet richtig erstellen
                trainingSet.addRow(
                        qValueEntry.mergedInputArray,
                        qValueEntry.binary_updateQVaule);
            }
            return trainingSet;
        }

    }

    public static class QValueEntry {
        double xPos,yPos;
        double action;
        double updatedQValue;

        //TODO: einteilung hier vornehmen
        // 4bit damit jede action eindeutig zugeordnert werden kann
        public static final int actionsBits = 4;
        double[] binary_actions;
        // 6bit  64
        double[] binary_xPos;
        public static final int positionBits = 6;
        // 6bit  64
        double[] binary_yPos;
        // 8bit 256
        double[] binary_updateQVaule;
        public static final int qValueBits = 8;

        double[] mergedInputArray;


        public QValueEntry(double xPos, double yPos, double action, double updatedQValue) {
            this.xPos = xPos;
            this.yPos = yPos;
            this.action = action;
            this.updatedQValue = updatedQValue;
        }

        public void normalize(Normalizable func) {
            func.setQValueEntry(this);
            func.normalize();
        }
    }

    public interface Normalizable {
        void normalize();
        void setQValueEntry(QValueEntry qValueEntry);
    }

    public static class NormalizerX1 implements Normalizable {
        QValueEntry qValueEntry;

        @Override
        public void normalize() {
            //TODO: hier die normalisierung durchführen

//            setAction(qValueEntry.action);
            qValueEntry.binary_actions = new double[QValueEntry.actionsBits];
            qValueEntry.binary_actions[(int)qValueEntry.action] = 1;
            qValueEntry.binary_xPos = createBinaryArray(QValueEntry.positionBits, qValueEntry.xPos);
            qValueEntry.binary_yPos = createBinaryArray(QValueEntry.positionBits, qValueEntry.yPos);
            qValueEntry.binary_updateQVaule = createBinaryArray(QValueEntry.qValueBits, qValueEntry.updatedQValue);
            mergeArrays();
        }

        private void mergeArrays() {
            qValueEntry.mergedInputArray = new double[InputNeuronSize];
            System.arraycopy(qValueEntry.binary_xPos,0,qValueEntry.mergedInputArray,0,QValueEntry.positionBits);
            System.arraycopy(qValueEntry.binary_yPos,0,qValueEntry.mergedInputArray,QValueEntry.positionBits,QValueEntry.positionBits);
            System.arraycopy(qValueEntry.binary_actions,0,qValueEntry.mergedInputArray,2*QValueEntry.positionBits,QValueEntry.actionsBits);
        }

        private double[] createBinaryArray(int bitcount, double number) {
            double[] result = new double[bitcount];
            char[] binaryString = Integer.toBinaryString((int) (number + 0.5)).toCharArray();
            int offset = 0;
            if (binaryString.length < bitcount) {
                offset = bitcount - binaryString.length;
            }

            for (int i = binaryString.length-1; 0 <= i; i--) {
                if (binaryString[i] == 49) {
                    result[i+offset] = 1;
                }else {
                    result[i+offset] = 0;
                }

            }
            return result;
        }
        @Override
        public void setQValueEntry(QValueEntry qValueEntry) {
            this.qValueEntry = qValueEntry;
        }
    }

}

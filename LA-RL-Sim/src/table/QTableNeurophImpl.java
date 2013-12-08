package table;

import org.neuroph.core.data.DataSet;
import org.neuroph.nnet.MultiLayerPerceptron;
import org.neuroph.util.TransferFunctionType;

import java.util.*;
import java.util.concurrent.ArrayBlockingQueue;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 08.12.13
 * Time: 13:32
 */
public class QTableNeurophImpl implements QTable {
    MultiLayerPerceptron network;
    private static final int InputNeuronSize =QValueEntry.actionsBits + QValueEntry.positionBits*2;
    private static final int OutputNeuronSize = QValueEntry.qValueBits;
    Queue<QValueEntry> history = new ArrayBlockingQueue<QValueEntry>(16);

    TrainingPatterns trainingPat = new TrainingPatterns();

    private static final double tausend = 1000;

    double[][][] table;

    private int width,height, numberOfActions;

    private void trainNet(Object[] history) {
        //TODO default training hier anpassen
        final int ITERATIONMAX = 3;
        final double MAXERROR = 0.15;
        trainNet(history,MAXERROR,ITERATIONMAX);


    }
    private void trainNet(Object[] history,double maxerror,int maxiteration) {
        DataSet trainingSet = trainingPat.createTrainingPatterns(history);
        System.out.println("trainingSet Size: "+trainingSet.size());
        network.getLearningRule().setMaxIterations(maxiteration);
//        network.getLearningRule().setMaxError(maxerror);
        network.getLearningRule().setLearningRate(0.0005);
        System.out.println("trainingSet isIterationLimited: "+network.getLearningRule().isIterationsLimited());
        network.learn(trainingSet);
    }

    @Override
    public void initialize(int width, int height, int numberOfActions) {
        this.width = width;
        this.height = height;
        this.numberOfActions = numberOfActions;

        table = new double[width][height][numberOfActions];
        network = new MultiLayerPerceptron(TransferFunctionType.LINEAR,InputNeuronSize,48,24,OutputNeuronSize);
//        initNetwork();

    }

    @Override
    public void updateQValue(int xPos, int yPos, int currentAction, double qValue) {
        QValueEntry qValueEntry = new QValueEntry(xPos,yPos,currentAction,qValue);
        qValueEntry.normalize(new NormalizerX1());
//        history.clear();
        if (history.offer(qValueEntry)) {

        }else {
            history.poll();
            history.offer(qValueEntry);
        }
        trainNet( history.toArray());
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


    private class TrainingPatterns {

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

        private void normalize(Normalizable func) {
            func.setQValueEntry(this);
            func.normalize();
        }
    }

    private interface Normalizable {
        void normalize();
        void setQValueEntry(QValueEntry qValueEntry);
    }

    private static class NormalizerX1 implements Normalizable {
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

    public static void main(String[] args) {
        System.out.println(Integer.toBinaryString(9));

        QValueEntry qValueEntry = new QValueEntry(33,12,3,45);
        qValueEntry.normalize(new NormalizerX1());
        System.out.println("qwe");
        double value = 0;
        for (int i = qValueEntry.binary_updateQVaule.length-1; 0 < i; i--) {
            value += (int)(qValueEntry.binary_updateQVaule[i]+0.5)*Math.pow(2, qValueEntry.binary_updateQVaule.length-1-i);
        }
        System.out.println(value);

    }
}

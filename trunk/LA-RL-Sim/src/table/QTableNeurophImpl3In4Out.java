package table;

import org.neuroph.core.data.DataSet;
import org.neuroph.nnet.MultiLayerPerceptron;
import org.neuroph.util.TransferFunctionType;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Queue;
import java.util.concurrent.ArrayBlockingQueue;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 08.12.13
 * Time: 13:32
 */
public class QTableNeurophImpl3In4Out implements QTable {
    MultiLayerPerceptron network;
    private static final int InputNeuronSize =  QValueEntry.positionBits*2;
    private static final int OutputNeuronSize = QValueEntry.qValueBits*4;
    Queue<QValueEntry> history = new ArrayBlockingQueue<QValueEntry>(12);

    TrainingPatterns trainingPat = new TrainingPatterns();


    double[][][] table;

    private int width,height, numberOfActions;

    private void trainNet(Object[] history) {
        //TODO default training hier anpassen
        final int ITERATIONMAX = 6;
        final double MAXERROR = 0.05;
        trainNet(history,MAXERROR,ITERATIONMAX);


    }
    private void trainNet(Object[] history,double maxerror,int maxiteration) {
        DataSet trainingSet = trainingPat.createTrainingPatterns(history);
        System.out.println("trainingSet Size: "+trainingSet.size());
        network.getLearningRule().setMaxIterations(maxiteration);
        network.getLearningRule().setMaxError(maxerror);
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
        network = new MultiLayerPerceptron(TransferFunctionType.TANH,InputNeuronSize,48,24,OutputNeuronSize);
        initNetwork();

    }

    private void initNetwork() {
        List<QValueEntry> initValues = new ArrayList<QValueEntry>();
        NormalizerX1 normalizerX1 = new NormalizerX1();
        QValueEntry qValueEntry;
        for (int x = 0; x < width; x++) {
            for (int y = 0; y < height; y++) {
                for (int action = 0; action < numberOfActions; action++) {
                    qValueEntry = new QValueEntry(x,y,0,0,0,0);
                    qValueEntry.normalize(normalizerX1);
                    initValues.add(qValueEntry);
                }
            }
        }
        System.out.println(new Date(System.currentTimeMillis())+": started init training");
        trainNet(initValues.toArray(), 0.0002, 30000);
        System.out.println(new Date(System.currentTimeMillis()) + ": finished init training");

    }
    double[] getQValuesAsDoubleArray(int xPos, int yPos) {
        double[] output = getQValues(xPos, yPos);
        double[] result = new double[4];
        result[0] = binaryArrayToDouble(getBinaryQValueArrayByIndex(output, 0));
        result[1] = binaryArrayToDouble(getBinaryQValueArrayByIndex(output, 1));
        result[2] = binaryArrayToDouble(getBinaryQValueArrayByIndex(output, 2));
        result[3] = binaryArrayToDouble(getBinaryQValueArrayByIndex(output, 3));
        return result;

    }
    @Override
    public void updateQValue(int xPos, int yPos, int currentAction, double qValue) {
        final double[] currentQValues = getQValuesAsDoubleArray(xPos, yPos);
        currentQValues[currentAction] = qValue;
        QValueEntry qValueEntry = new QValueEntry(xPos, yPos,currentQValues[0],currentQValues[1],currentQValues[2],currentQValues[3]);
        qValueEntry.normalize(new NormalizerX1());
//        history.clear();
        if (history.offer(qValueEntry)) {

        } else {
            history.poll();
            history.offer(qValueEntry);
        }
        trainNet(history.toArray());
    }

    private double[] getQValues(int xPos,int yPos) {

        QValueEntry qValueEntry = new QValueEntry(xPos,yPos,0,0);
        qValueEntry.normalize(new NormalizerX1());
        //TODO input values richtig machen
        network.setInput(qValueEntry.mergedInputArray);
        network.calculate();
        return network.getOutput();
    }

    private double[] getBinaryQValueArrayByIndex(double[] output, int index) {
        double[] binaryQValue = new double[QValueEntry.qValueBits];
        System.arraycopy(output, index * QValueEntry.qValueBits, binaryQValue, 0, QValueEntry.qValueBits);
        return binaryQValue;
    }

    @Override
    public int getBestAction(int xPos, int yPos) {
        double[] qValues = getQValues(xPos, yPos);
        double tmp, min = binaryArrayToDouble(getBinaryQValueArrayByIndex(qValues, 0));
        int bestAction = 0;

        for (int action = 1; action < 4; action++) {
            tmp = binaryArrayToDouble(getBinaryQValueArrayByIndex(qValues, action));
            if (min > tmp) {
                min = tmp;
                bestAction = action;
            }
        }
        return bestAction;
    }

    @Override
    public double getMinQValue(int xPos, int yPos) {
        double[] qValues = getQValues(xPos, yPos);
        double tmp, min = binaryArrayToDouble(getBinaryQValueArrayByIndex(qValues, 0));
        int bestAction = 0;

        for (int action = 1; action < 4; action++) {
            tmp = binaryArrayToDouble(getBinaryQValueArrayByIndex(qValues, action));
            if (min > tmp) {
                min = tmp;
                bestAction = action;
            }
        }
        return min;
    }

    @Override
    public double getQValue(int xPos, int yPos, int action) {
        final double[] output = getQValues(xPos, yPos);
        return binaryArrayToDouble(getBinaryQValueArrayByIndex(output,action));
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
                        qValueEntry.mergedOutputArray);
            }
            return trainingSet;
        }

    }

    public static class QValueEntry {
        double xPos,yPos;
        double action;
        double updatedQValue;
        double q_action0,q_action1,q_action2, q_action3;
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
        double[] binary_q_action0;
        double[] binary_q_action1;
        double[] binary_q_action2;
        double[] binary_q_action3;
        public static final int qValueBits = 8;

        double[] mergedInputArray;
        double[] mergedOutputArray;


        public QValueEntry(double xPos, double yPos, double action, double updatedQValue) {
            this.xPos = xPos;
            this.yPos = yPos;
            this.action = action;
            this.updatedQValue = updatedQValue;
        }

        public QValueEntry(double xPos, double yPos, double q_action0, double q_action1, double q_action2, double q_action3) {
            this.xPos = xPos;
            this.yPos = yPos;
            this.q_action0 = q_action0;
            this.q_action1 = q_action1;
            this.q_action2 = q_action2;
            this.q_action3 = q_action3;
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

            qValueEntry.binary_xPos = createBinaryArray(QValueEntry.positionBits, qValueEntry.xPos);
            qValueEntry.binary_yPos = createBinaryArray(QValueEntry.positionBits, qValueEntry.yPos);
            qValueEntry.binary_q_action0 = createBinaryArray(QValueEntry.qValueBits, qValueEntry.q_action0);
            qValueEntry.binary_q_action1 = createBinaryArray(QValueEntry.qValueBits, qValueEntry.q_action1);
            qValueEntry.binary_q_action2 = createBinaryArray(QValueEntry.qValueBits, qValueEntry.q_action2);
            qValueEntry.binary_q_action3 = createBinaryArray(QValueEntry.qValueBits, qValueEntry.q_action3);
            mergeArrays();
        }

        private void mergeArrays() {
            qValueEntry.mergedInputArray = new double[InputNeuronSize];
            System.arraycopy(qValueEntry.binary_xPos,0,qValueEntry.mergedInputArray,0, QValueEntry.positionBits);
            System.arraycopy(qValueEntry.binary_yPos,0,qValueEntry.mergedInputArray, QValueEntry.positionBits, QValueEntry.positionBits);

            qValueEntry.mergedOutputArray = new double[OutputNeuronSize];
            System.arraycopy(qValueEntry.binary_q_action0,0,qValueEntry.mergedOutputArray,0, QValueEntry.qValueBits);
            System.arraycopy(qValueEntry.binary_q_action1,0,qValueEntry.mergedOutputArray,QValueEntry.qValueBits, QValueEntry.qValueBits);
            System.arraycopy(qValueEntry.binary_q_action2,0,qValueEntry.mergedOutputArray,2*QValueEntry.qValueBits, QValueEntry.qValueBits);
            System.arraycopy(qValueEntry.binary_q_action3,0,qValueEntry.mergedOutputArray,3*QValueEntry.qValueBits, QValueEntry.qValueBits);

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

        QValueEntry qValueEntry = new QValueEntry(33,12,1,128,1,128);
        qValueEntry.normalize(new NormalizerX1());
        System.out.println("qwe");


    }
}

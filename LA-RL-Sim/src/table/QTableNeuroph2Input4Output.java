package table;

import org.neuroph.core.data.DataSet;
import org.neuroph.nnet.MultiLayerPerceptron;
import org.neuroph.util.TransferFunctionType;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Alexander Breitenstein
 * Date: 29.12.13
 * Time: 07:26
 */
public class QTableNeuroph2Input4Output implements QTable {
    MultiLayerPerceptron network;
    private static final boolean DEBUG = true;
    private static final int InputNeuronSize =2;
    private static final int OutputNeuronSize = 4;
    List<QValueEntry> history = new ArrayList<QValueEntry>();
    List<List<QValueEntry>> historyList = new ArrayList<List<QValueEntry>>();
    TrainingPatterns trainingPat = new TrainingPatterns();

    private static final double tausend = 1000;

    double[][][] table;

    private int width,height, numberOfActions;

    private void trainNet(List<List<QValueEntry>> historyList) {

        final int ITERATIONMAX = 100000;
        final double MAXERROR = 0.005;
        trainNet(historyList,MAXERROR,ITERATIONMAX);


    }
    private void trainNet(List<List<QValueEntry>> historyList,double maxerror,int maxiteration) {
        DataSet trainingSet = trainingPat.createTrainingPatterns(historyList);
        System.out.println("trainingSet Size: "+trainingSet.size());
//        network.getLearningRule().setMaxIterations(maxiteration);
        network.getLearningRule().setMaxError(maxerror);
        System.out.println("trainingSet isIterationLimited: "+network.getLearningRule().isIterationsLimited());
        network.learn(trainingSet);
    }

    private void initNetwork() {
        NormalizerX1 normalizerX1 = new NormalizerX1();
        QValueEntry qValueEntry;
        for (int x = 0; x < width; x++) {
            for (int y = 0; y < height; y++) {
                List<QValueEntry> initValues = new ArrayList<QValueEntry>();
                for (int action = 0; action < numberOfActions; action++) {
                    qValueEntry = new QValueEntry(x,y,action,0.0);
                    qValueEntry.normalize(normalizerX1);
                    initValues.add(qValueEntry);
                }
                historyList.add(initValues);

            }
        }
        System.out.println(new Date(System.currentTimeMillis())+": started init training");
        trainNet(historyList, 0.0002, 30000);
        System.out.println(new Date(System.currentTimeMillis()) + ": finished init training");

    }

    @Override
    public void initialize(int width, int height, int numberOfActions) {
        this.width = width;
        this.height = height;
        this.numberOfActions = numberOfActions;

        table = new double[width][height][numberOfActions];

        network = new MultiLayerPerceptron(TransferFunctionType.LINEAR,InputNeuronSize,24,12,6,OutputNeuronSize);
        initNetwork();

    }

    @Override
    public void updateQValue(int xPos, int yPos, int currentAction, double qValue) {
        QValueEntry qValueEntry = new QValueEntry(xPos, yPos, currentAction, qValue);

        QValueEntry[] qValueEntries = new QValueEntry[4];
        List<QValueEntry> qValueEntryList = new ArrayList<QValueEntry>();
        for (int i = 0; i < 4; i++) {
            if (currentAction != i) {
                qValueEntries[i] = new QValueEntry(xPos, yPos, i, getQValue(xPos, yPos, i));
            }else{
                qValueEntries[currentAction] = new QValueEntry(xPos, yPos, currentAction, qValue);
            }
        }

        for (QValueEntry valueEntry : qValueEntries) {
            valueEntry.normalize(new NormalizerX1());
            qValueEntryList.add(valueEntry);
        }


        historyList.add(qValueEntryList);
        trainNet(historyList);
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

        network.setInput(new double[]{qValueEntry.xPos,qValueEntry.yPos});
        network.calculate();
        final double[] output = network.getOutput();

        return output[action] * tausend;

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

        public DataSet createTrainingPatterns(List<List<QValueEntry>> historyList) {
            DataSet trainingSet = new DataSet(InputNeuronSize, OutputNeuronSize);

            QValueEntry qValueEntry0,qValueEntry1,qValueEntry2,qValueEntry3;
            for (int i = 0; i < historyList.size(); i+=4) {
                qValueEntry0 = historyList.get(i).get(0);
                qValueEntry1 = historyList.get(i).get(1);
                qValueEntry2 = historyList.get(i).get(2);
                qValueEntry3 = historyList.get(i).get(3);

                trainingSet.addRow(
                        new double[]{qValueEntry0.xPos,qValueEntry0.yPos},
                        new double[]{
                                qValueEntry0.updatedQValue,
                                qValueEntry1.updatedQValue,
                                qValueEntry2.updatedQValue,
                                qValueEntry3.updatedQValue
                        });
            }
            return trainingSet;
        }

    }

    public class QValueEntry {
        double xPos,yPos;
        double action;
        double action0,action1,action2, action3;
        double updatedQValue;

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

    private class NormalizerX1 implements Normalizable {
        QValueEntry qValueEntry;

        final double _width = 1 / (double) width;
        final double _height = 1 / (double) height;
        final double _actions = 1 / (double) numberOfActions;
        static final double _100 = 1/tausend;
        @Override
        public void normalize() {
            qValueEntry.xPos *= _width;
            qValueEntry.yPos *= _height;
            qValueEntry.action *= _actions;
            qValueEntry.updatedQValue *= _100;
//            setAction(qValueEntry.action);
        }

        private void setAction(double action) {
            qValueEntry.action0 = 0;
            qValueEntry.action1 = 0;
            qValueEntry.action2 = 0;
            qValueEntry.action3 = 0;
            switch ((int)action) {
                case 0: qValueEntry.action0 = 1;break;
                case 1: qValueEntry.action1 = 1;break;
                case 2: qValueEntry.action2 = 1;break;
                case 3: qValueEntry.action3 = 1;break;
            }
        }
        @Override
        public void setQValueEntry(QValueEntry qValueEntry) {
            this.qValueEntry = qValueEntry;
        }
    }
}
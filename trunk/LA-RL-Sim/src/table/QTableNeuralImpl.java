package table;

import org.encog.engine.network.activation.ActivationGaussian;
import org.encog.engine.network.activation.ActivationLinear;
import org.encog.engine.network.activation.ActivationSigmoid;
import org.encog.ml.data.MLData;
import org.encog.ml.data.MLDataPair;
import org.encog.ml.data.MLDataSet;
import org.encog.ml.data.basic.BasicMLData;
import org.encog.ml.data.basic.BasicMLDataPair;
import org.encog.ml.data.basic.BasicMLDataSet;
import org.encog.neural.networks.BasicNetwork;
import org.encog.neural.networks.layers.BasicLayer;
import org.encog.neural.networks.training.propagation.back.Backpropagation;
import org.encog.neural.networks.training.propagation.resilient.ResilientPropagation;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 01.12.13
 * Time: 22:09
 */
public class QTableNeuralImpl implements QTable {
    BasicNetwork network;
    private static final boolean DEBUG = true;
    private static final int InputNeuronSize =6;
    private static final int OutputNeuronSize = 1;
    List<QValueEntry> history = new ArrayList<QValueEntry>();
    Map<String, QValueEntry> historyMap = new HashMap<String, QValueEntry>();
    TrainingPatterns trainingPat = new TrainingPatterns();

    private static final double tausend = 1000;

    double[][][] table;

    private int width,height, numberOfActions;

    private void trainNet(List<QValueEntry> history) {

        TrainingPatterns trainingPatterns = trainingPat.createTrainingPatterns(history);

        //historyMapCode Start -----
//        TrainingPatterns trainingPatterns = trainingPat.createTrainingPatterns(new ArrayList<QValueEntry>(historyMap.values()));
        //historyMapCode END -----

        final MLDataSet trainingSet = new BasicMLDataSet(trainingPatterns.trainingInput, trainingPatterns.trainingOutput);
        final ResilientPropagation train = new ResilientPropagation(network, trainingSet);
        final int ITERATIONMAX = 6000;

//        System.out.println("## Start Training ##");
        int iteration = 0;
        do {
            train.iteration();

            iteration++;
//               if (iteration % 1 == 0) System.out.println("Errosrate: " + train.getError() + ", Training iteration: "+iteration);

        } while (train.getError() > 0.07 && iteration < ITERATIONMAX);

        train.finishTraining();
        System.out.println("Länge der History: " + history.size());
    }

    @Override
    public void initialize(int width, int height, int numberOfActions) {
        this.width = width;
        this.height = height;
        this.numberOfActions = numberOfActions;

        table = new double[width][height][numberOfActions];

        network = new BasicNetwork();
        network.addLayer(new BasicLayer(null,true,InputNeuronSize));
        network.addLayer(new BasicLayer(new ActivationLinear(),true,4));
        network.addLayer(new BasicLayer(new ActivationLinear(),true,3));
        network.addLayer(new BasicLayer(new ActivationLinear(),true,2));
        network.addLayer(new BasicLayer(new ActivationLinear(),true,OutputNeuronSize));
        network.getStructure().finalizeStructure();
        network.reset();


    }

    @Override
    public void updateQValue(int xPos, int yPos, int currentAction, double qValue) {
        QValueEntry qValueEntry = new QValueEntry(xPos,yPos,currentAction,qValue);

        qValueEntry.normalize(new NormalizerX1());

        //historyMapCode START -----
//        final String key = getKey(xPos, yPos, currentAction);
//        historyMap.put(key, qValueEntry);
        //historyMapCode END -----

        history.add(qValueEntry);
        //System.err.println(history.size());
        network.reset();
        trainNet(history);
    }

    private double[] getQValues(int xPos,int yPos) {
        double[] result = new double[numberOfActions];
        for (int action = 0; action < numberOfActions; action++) {
            result[action] = getQValue(xPos, yPos, action);
        }
//        System.out.println("x: "+xPos+", y: "+yPos+", r: ["+result[0]/tausend+", "+result[1]/tausend+", "+result[2]/tausend+", "+result[3]/tausend+"];");
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

        MLDataPair pair = new BasicMLDataPair(new BasicMLData(new double[]{
                qValueEntry.xPos,
                qValueEntry.yPos,
                qValueEntry.action0,
                qValueEntry.action1,
                qValueEntry.action2,
                qValueEntry.action3
        }));
        final MLData outPut = network.compute(pair.getInput());

        //System.err.println(outPut.getData()[0]*100000);
//        return outPut.getData()[0] * tausend;
        return outPut.getData()[0];
    }

    private String getKey(int xPos, int yPos, int action) {
        return String.valueOf(xPos).concat(String.valueOf(yPos)).concat(String.valueOf(action));
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
        double trainingInput[][];
        double trainingOutput[][];

        public TrainingPatterns createTrainingPatterns(List<QValueEntry> history) {
            final int sizeOfHistory = history.size();
            trainingInput = new double[sizeOfHistory][InputNeuronSize];
            trainingOutput = new double[sizeOfHistory][OutputNeuronSize];

            QValueEntry qValueEntry;
            for (int i = 0; i < sizeOfHistory; i++) {
                qValueEntry = history.get(i);
                trainingInput[i] = new double[]{
                        qValueEntry.xPos,
                        qValueEntry.yPos,
                        qValueEntry.action0,
                        qValueEntry.action1,
                        qValueEntry.action2,
                        qValueEntry.action3
                };
                trainingOutput[i] = new double[]{qValueEntry.updatedQValue};
            }
            return this;
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
//            qValueEntry.xPos *= _width;
//            qValueEntry.yPos *= _height;
//            qValueEntry.action *= _actions;
            setAction(qValueEntry.action);
//            qValueEntry.updatedQValue *= _100;
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

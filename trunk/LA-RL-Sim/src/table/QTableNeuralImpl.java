package table;

import org.encog.engine.network.activation.ActivationSigmoid;
import org.encog.ml.data.MLData;
import org.encog.ml.data.MLDataPair;
import org.encog.ml.data.MLDataSet;
import org.encog.ml.data.basic.BasicMLData;
import org.encog.ml.data.basic.BasicMLDataPair;
import org.encog.ml.data.basic.BasicMLDataSet;
import org.encog.neural.networks.BasicNetwork;
import org.encog.neural.networks.layers.BasicLayer;
import org.encog.neural.networks.training.propagation.resilient.ResilientPropagation;

import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 01.12.13
 * Time: 22:09
 */
public class QTableNeuralImpl implements QTable {
    BasicNetwork network;
    private static final boolean DEBUG = true;
    private static final int InputNeuronSize = 3;
    private static final int OutputNeuronSize = 1;
    List<QValueEntry> history = new ArrayList<QValueEntry>();
    TrainingPatterns trainingPat = new TrainingPatterns();

    private int width,height, numberOfActions;

    private void trainNet(List<QValueEntry> history) {

        TrainingPatterns trainingPatterns = trainingPat.createTrainingPatterns(history);
        final MLDataSet trainingSet = new BasicMLDataSet(trainingPatterns.trainingInput, trainingPatterns.trainingOutput);
        final ResilientPropagation train = new ResilientPropagation(network, trainingSet);
        final int ITERATIONMAX = 1;

//        System.out.println("## Start Training ##");
        int iteration = 0;
        do {
            train.iteration();

            iteration++;
            //   if (iteration % 1 == 0) System.out.println("Errosrate: " + train.getError() + ", Training iteration: "+iteration);

        } while (train.getError() > 0.01 && iteration < ITERATIONMAX);

        train.finishTraining();
    }

    @Override
    public void initialize(int width, int height, int numberOfActions) {
        this.width = width;
        this.height = height;
        this.numberOfActions = numberOfActions;

        network = new BasicNetwork();
        network.addLayer(new BasicLayer(null,true,InputNeuronSize));
        network.addLayer(new BasicLayer(new ActivationSigmoid(),true,12));
        network.addLayer(new BasicLayer(new ActivationSigmoid(),true,3));
        network.addLayer(new BasicLayer(new ActivationSigmoid(),true,OutputNeuronSize));
        network.getStructure().finalizeStructure();
        network.reset();


    }

    @Override
    public void updateQValue(int xPos, int yPos, int currentAction, double qValue) {
        QValueEntry qValueEntry = new QValueEntry(xPos,yPos,currentAction,qValue);

        qValueEntry.normalize(new NormalizerX1());
        history.add(qValueEntry);
        //System.err.println(history.size());
        trainNet(history);
    }

    @Override
    public int getBestAction(int xPos, int yPos) {
        throw new UnsupportedOperationException("QTableNeuralImpl :: getBestAction not implemented yet");
    }

    @Override
    public double getMinQValue(int xPos, int yPos) {
        throw new UnsupportedOperationException("QTableNeuralImpl :: getMinQValue not implemented yet");
    }

    @Override
    public double getQValue(int xPos, int yPos, int action) {
        QValueEntry qValueEntry = new QValueEntry(xPos,yPos,action,0);
        qValueEntry.normalize(new NormalizerX1());

        MLDataPair pair = new BasicMLDataPair(new BasicMLData(new double[]{
                qValueEntry.xPos,
                qValueEntry.yPos,
                qValueEntry.action
        }));
        final MLData outPut = network.compute(pair.getInput());

        //System.err.println(outPut.getData()[0]*100000);
        return outPut.getData()[0] * 100;
    }

    @Override
    public double[][][] getCompleteTable() {
        throw new UnsupportedOperationException("QTableNeuralImpl :: getCompleteTable not implemented yet");
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
                        qValueEntry.action
                };
                trainingOutput[i] = new double[]{qValueEntry.updatedQValue};
            }
            return this;
        }
    }

    public class QValueEntry {
        double xPos,yPos;
        double action;
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
        static final double _100 = 1/100.0;
        @Override
        public void normalize() {
            qValueEntry.xPos *= _width;
            qValueEntry.yPos *= _height;
            qValueEntry.action *= _actions;
            qValueEntry.updatedQValue *= _100;
        }

        @Override
        public void setQValueEntry(QValueEntry qValueEntry) {
            this.qValueEntry = qValueEntry;
        }
    }


}

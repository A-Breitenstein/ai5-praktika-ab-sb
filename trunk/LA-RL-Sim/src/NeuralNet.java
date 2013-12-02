package qNet;

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

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: abg667
 * Date: 18.11.13
 * Time: 10:02
 * To change this template use File | Settings | File Templates.
 */

//public class NeuralNet implements IQTable {
//    BasicNetwork network;
//    private static final boolean DEBUG = true;
//    final int InputNeuronSize = 9;
//    List<QValueEntry> history = new ArrayList<QValueEntry>();
//    TrainingPatterns trainingPat = new TrainingPatterns();
//
//     // default Action, max Actions 45:{0-44}
//     private int defaultAction = 37;
//
//    private void trainNet(List<QValueEntry> history) {
//
//        TrainingPatterns trainingPatterns = trainingPat.createTrainingPatterns(history);
//        final MLDataSet trainingSet = new BasicMLDataSet(trainingPatterns.trainingInput, trainingPatterns.trainingOutput);
//        final ResilientPropagation train = new ResilientPropagation(network, trainingSet);
//        final int ITERATIONMAX = 1;
//
////        System.out.println("## Start Training ##");
//        int iteration = 0;
//        do {
//            train.iteration();
//
//            iteration++;
//         //   if (iteration % 1 == 0) System.out.println("Errosrate: " + train.getError() + ", Training iteration: "+iteration);
//
//        } while (train.getError() > 0.01 && iteration < ITERATIONMAX);
//
//        train.finishTraining();
//    }
//
//    @Override
//    public void updateQValue(double speed, double angleToTrackAxis, double leftSideDistance, double leftFrontDistance, double rightSideDistance, double rightFrontDistance, int action, double updatedQValue) {
//        QValueEntry qValueEntry = new QValueEntry(speed, angleToTrackAxis, leftSideDistance, leftFrontDistance, rightSideDistance, rightFrontDistance, action, updatedQValue);
//
//        qValueEntry.normalize(new NormalizerX1());
//        history.add(qValueEntry);
//        //System.err.println(history.size());
//        trainNet(history);
//    }
//
//    @Override
//    public double getQValue(double speed, double angleToTrackAxis, double leftSideDistance, double leftFrontDistance, double rightSideDistance, double rightFrontDistance, int action) {
//        QValueEntry qValueEntry = new QValueEntry(speed, angleToTrackAxis, leftSideDistance, leftFrontDistance, rightSideDistance, rightFrontDistance, action, 1000000);
//        qValueEntry.normalize(new NormalizerX1());
//
//        MLDataPair pair = new BasicMLDataPair(new BasicMLData(new double[]{qValueEntry.speed, qValueEntry.angleToTrackAxis, qValueEntry.leftSideDistance, qValueEntry.leftFrontDistance, qValueEntry.rightSideDistance, qValueEntry.rightFrontDistance, qValueEntry.action}));
//        final MLData outPut = network.compute(pair.getInput());
//
//        //System.err.println(outPut.getData()[0]*100000);
//        return outPut.getData()[0]*100000;
//    }
//
//    @Override
//    public Double[] getQValues(double speed, double angleToTrackAxis, double leftSideDistance, double leftFrontDistance, double rightSideDistance, double rightFrontDistance) {
//        final int maxActions = 45;
//        Double[] doubles = new Double[maxActions];
//
//        for (int i = 0; i < 45; i++) {
//            doubles[i] = getQValue(speed, angleToTrackAxis,leftSideDistance,leftFrontDistance,rightSideDistance,rightFrontDistance,i);
//        }
//
//        return doubles;
//    }
//
//    @Override
//    public int getBestAction(double speed, double angleToTrackAxis, double leftSideDistance, double leftFrontDistance, double rightSideDistance, double rightFrontDistance) {
//
//        Double[] qValues = getQValues(speed, angleToTrackAxis, leftSideDistance, leftFrontDistance, rightSideDistance, rightFrontDistance);
//
//        int action = defaultAction;
//
//
//
//        double bestReward = qValues[0];
//        //action = 0;
//        for (int i = 1; i < qValues.length; i++) {
//            if (qValues[i] >= bestReward) {
//                action = i;
//                bestReward = qValues[i];
//            }
//        }
//        return action;
//    }
//
//    @Override
//    public void importTable(Path source) {
//        network = new BasicNetwork();
//        network.addLayer(new BasicLayer(null,true,7));
//        network.addLayer(new BasicLayer(new ActivationSigmoid(),true,12));
//        network.addLayer(new BasicLayer(new ActivationSigmoid(),true,3));
//        network.addLayer(new BasicLayer(new ActivationSigmoid(),true,1));
//        network.getStructure().finalizeStructure();
//        network.reset();
//    }
//
//    @Override
//    public void saveSortedTable(Path destination) {
//        //To change body of implemented methods use File | Settings | File Templates.
//    }
//
//    public class QValueEntry {
//        double speed,
//                angleToTrackAxis,
//                leftSideDistance,
//                leftFrontDistance,
//                rightSideDistance,
//                rightFrontDistance;
//        double action;
//        double updatedQValue;
//
//        private QValueEntry(double speed, double angleToTrackAxis, double leftSideDistance, double leftFrontDistance, double rightSideDistance, double rightFrontDistance, int action, double updatedQValue) {
//            this.speed = speed;
//            this.angleToTrackAxis = angleToTrackAxis;
//            this.leftSideDistance = leftSideDistance;
//            this.leftFrontDistance = leftFrontDistance;
//            this.rightSideDistance = rightSideDistance;
//            this.rightFrontDistance = rightFrontDistance;
//            this.action = action;
//            this.updatedQValue = updatedQValue;
//        }
//        private void normalize(Normalizable func) {
//            func.setQValueEntry(this);
//            func.normalize();
//        }
//    }
//
//    private class TrainingPatterns {
//        double trainingInput[][];
//        double trainingOutput[][];
//
//        public TrainingPatterns createTrainingPatterns(List<QValueEntry> history) {
//            final int sizeOfHistory = history.size();
//            trainingInput = new double[sizeOfHistory][InputNeuronSize];
//            trainingOutput = new double[sizeOfHistory][InputNeuronSize];
//
//            QValueEntry qValueEntry;
//            for (int i = 0; i < sizeOfHistory; i++) {
//                qValueEntry = history.get(i);
//                trainingInput[i] = new double[]{qValueEntry.speed,
//                                                qValueEntry.angleToTrackAxis,
//                                                qValueEntry.leftSideDistance,
//                                                qValueEntry.leftFrontDistance,
//                                                qValueEntry.rightSideDistance,
//                                                qValueEntry.rightFrontDistance,
//                                                qValueEntry.action};
//                trainingOutput[i] = new double[]{qValueEntry.updatedQValue};
//            }
//            return this;
//        }
//    }
//
//    private interface Normalizable {
//        void normalize();
//        void setQValueEntry(NeuralNet.QValueEntry qValueEntry);
//    }
//
//    private class NormalizerX1 implements Normalizable {
//        QValueEntry qValueEntry;
//
//        final double fourHundred = 1 / 400.d;
//        final double twoHundred = 1 / 200.d;
//        final double fortyFive = 1/45.d;
//        final double hundredThousand = 1/100000;
//        final double pi = 1 / Math.PI;
//
//        @Override
//        public void normalize() {
//            qValueEntry.speed = qValueEntry.speed*fourHundred;
//            qValueEntry.angleToTrackAxis = qValueEntry.angleToTrackAxis*pi;
//            qValueEntry.leftSideDistance = qValueEntry.leftSideDistance*twoHundred;
//            qValueEntry.leftFrontDistance = qValueEntry.leftFrontDistance*twoHundred;
//            qValueEntry.rightSideDistance = qValueEntry.rightSideDistance*twoHundred;
//            qValueEntry.rightFrontDistance = qValueEntry.rightFrontDistance*twoHundred;
//            qValueEntry.action = qValueEntry.action*fortyFive;
//            qValueEntry.updatedQValue = qValueEntry.updatedQValue*hundredThousand;
//        }
//
//        @Override
//        public void setQValueEntry(QValueEntry qValueEntry) {
//            this.qValueEntry = qValueEntry;
//        }
//    }
//
//}
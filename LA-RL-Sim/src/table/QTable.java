package table;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 01.12.13
 * Time: 21:17
 */
public interface QTable {
    void initialize(int width,int height,int numberOfActions);

    void updateQValue(int xPos, int yPos, int currentAction, double qValue);

    int getBestAction(int xPos, int yPos);

    double getMinQValue(int xPos, int yPos);

    double getQValue(int xPos, int yPos, int action);

    double[][][] getCompleteTable();

}

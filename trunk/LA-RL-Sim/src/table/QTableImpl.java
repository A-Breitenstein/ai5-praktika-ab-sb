package table;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 01.12.13
 * Time: 21:38
 */
public class QTableImpl implements QTable {
    private int width,height, numberOfActions;
    double[][][] table;
    @Override
    public void initialize(int width, int height, int numberOfActions) {
        this.width = width;
        this.height = height;
        this.numberOfActions = numberOfActions;

        table = new double[width][height][numberOfActions];

        for (int i = 0; i < table.length; i++)
            for (int j = 0; j < table[i].length; j++)
                for (int k = 0; k < table[i][j].length; k++)
                    table[i][j][k] = 0;
    }

    @Override
    public void updateQValue(int xPos, int yPos, int currentAction, double qValue) {
        table[xPos][yPos][currentAction] = qValue;
    }

    @Override
    public int getBestAction(int xPos, int yPos) {
        double min = table[xPos][yPos][0];
        int bestAction = 0;
        for (int i=1;i<table[xPos][yPos].length;i++)
        {
            if (min>table[xPos][yPos][i])
            {
                min = table[xPos][yPos][i];
                bestAction = i;
            }
        }
        return bestAction;
    }

    @Override
    public double getMinQValue(int xPos, int yPos) {
            double min = table[xPos][yPos][0];
            int bestAction = 0;
            for (int i=0;i<table[xPos][yPos].length;i++)
            {
                if (min>table[xPos][yPos][i])
                {
                    min = table[xPos][yPos][i];
                    bestAction = i;
                }
            }
            return min;
    }

    @Override
    public double getQValue(int xPos, int yPos, int action) {
        return table[xPos][yPos][action];
    }

    @Override
    public double[][][] getCompleteTable() {
        return table;
    }
}

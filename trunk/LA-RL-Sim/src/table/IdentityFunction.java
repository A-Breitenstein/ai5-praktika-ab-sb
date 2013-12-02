package table;

import org.encog.engine.network.activation.ActivationFunction;

/**
 * Created with IntelliJ IDEA.
 * User: abg667
 * Date: 02.12.13
 * Time: 11:03
 * To change this template use File | Settings | File Templates.
 */
public class IdentityFunction implements ActivationFunction {
    @Override
    public void activationFunction(double[] d, int start, int size) {

    }

    @Override
    public double derivativeFunction(double b, double a) {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public boolean hasDerivative() {
        return false;  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public double[] getParams() {
        return new double[0];  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public void setParam(int index, double value) {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public String[] getParamNames() {
        return new String[0];  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public ActivationFunction clone() {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }
}

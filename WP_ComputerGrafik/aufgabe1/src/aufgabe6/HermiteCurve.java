package aufgabe6;

import javax.vecmath.Vector3d;

/**
 * Created with IntelliJ IDEA.
 * User: abg628
 * Date: 03.12.13
 * Time: 08:38
 * To change this template use File | Settings | File Templates.
 */
public class HermiteCurve implements Curve {

    private Vector3d[] points;

    private HermiteCurve(Vector3d v1, Vector3d v2, Vector3d v3, Vector3d v4) {
        points = new Vector3d[]{v1, v2, v3, v4};
    }

    public static HermiteCurve create(Vector3d v1, Vector3d v2, Vector3d v3, Vector3d v4) {
        return new HermiteCurve(v1, v2, v3, v4);
    }

    @Override
    public Vector3d eval(double value) {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public Vector3d derivative(double value) {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    private double evalBasisFunction(int num, double val) {

            return 0;
        }

    private double evalDerivative(int num, double val) {

        return 0;
    }

    }

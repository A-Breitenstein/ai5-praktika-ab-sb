package aufgabe6;

import javax.vecmath.Vector3d;

/**
 * Created with IntelliJ IDEA.
 * User: abg628
 * Date: 03.12.13
 * Time: 08:38
 * To change this template use File | Settings | File Templates.
 */
public class MonomialCurve implements Curve {

    private Vector3d[] controlPoints;

    private MonomialCurve(int size) {
        controlPoints = new Vector3d[size];
    }

    private MonomialCurve(Vector3d v1, Vector3d v2, Vector3d v3, Vector3d v4) {
        controlPoints = new Vector3d[]{v1, v2, v3, v4};
    }

    public static MonomialCurve create(int size) {
        return new MonomialCurve(size);
    }

    public static MonomialCurve create(Vector3d v1, Vector3d v2, Vector3d v3, Vector3d v4) {
        return new MonomialCurve(v1, v2, v3, v4);
    }

    @Override
    public Vector3d eval(double value) {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public Vector3d derivative(double value) {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public int getDegree() {
        return controlPoints.length;
    }

    public void setControlPoint(int index, Vector3d vector) {
        final int arrayLength = controlPoints.length;

        if (index > arrayLength)
            throw new IndexOutOfBoundsException("Angegebener Index beträgt:" + index + "größter index des Arrays beträgt:" + (arrayLength - 1));

        controlPoints[index] = vector;
    }

}

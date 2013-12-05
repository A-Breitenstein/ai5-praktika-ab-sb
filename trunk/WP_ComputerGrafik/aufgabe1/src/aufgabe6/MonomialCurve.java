package aufgabe6;

import javax.vecmath.Point3d;
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

    public MonomialCurve(Vector3d[] vectors) {
        controlPoints = vectors;
    }

    public static MonomialCurve create(int size) {
        return new MonomialCurve(size);
    }

    public static MonomialCurve create(Vector3d v1, Vector3d v2, Vector3d v3, Vector3d v4) {
        return new MonomialCurve(v1, v2, v3, v4);
    }
    public static MonomialCurve create(Vector3d[] vectors) {
        return new MonomialCurve(vectors);
    }

    @Override
    public Vector3d eval(double value) {
        Vector3d tangentV = new Vector3d();
        Vector3d temp;
        for (int i = 0; i < getDegree(); i++) {
            temp = new Vector3d(controlPoints[i]);

            temp.scale(Math.pow(value,i));

            tangentV.add(temp);
        }

        return tangentV;
    }

    @Override
    public Vector3d derivative(double value) {
        Vector3d tangentV = new Vector3d();
        Vector3d temp;
        double scalar;
        for (int i = 1; i < getDegree(); i++) {
            temp = new Vector3d(controlPoints[i]);

            scalar = ableitung(i, value);

            temp.scale(scalar);

            tangentV.add(temp);
        }

        return tangentV;
    }

    @Override
    public Vector3d[] getControllPoints() {
        return controlPoints;
    }

    private double ableitung(int z, double value) {
        return z * Math.pow(value, z - 1);
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

    public static Curve interpolate(Vector3d p0, Vector3d p1, Vector3d p2) {
        VectorRichtig p0_ = VectorRichtig.create(p0),
                      p1_ = VectorRichtig.create(p1),
                      p2_ = VectorRichtig.create(p2);
        VectorRichtig dc2, dc1, dc0;
        final double k_DH = 1.0 / -0.25;

        dc2 = p0_.scale(0.5).add(p1_).sub(p0_.add(p2_.scale(0.5)));
        dc1 = p0_.add(p2_.scale(0.25)).sub(p1_.add(p0_.scale(0.25)));
        dc0 = p0_.scale(0.25).sub(p0_.scale(0.5));


        return new MonomialCurve(new Vector3d[]{dc0.scale(k_DH).toVektorFalsch(),
                                                dc1.scale(k_DH).toVektorFalsch(),
                                                dc2.scale(k_DH).toVektorFalsch()
                                                });
    }

    public static Curve interpolate(Vector3d p0, Vector3d p1) {

        VectorRichtig vp0 = VectorRichtig.create(p0), vp1 = VectorRichtig.create(p1), dc0, dc1;
        final double k_DH = 1.0;

        dc0 = vp0;
        dc1 = vp1.sub(vp0);

        return new MonomialCurve(new Vector3d[]{dc0.scale(k_DH).toVektorFalsch(),
                                                dc1.scale(k_DH).toVektorFalsch()
                                               });
    }


    }

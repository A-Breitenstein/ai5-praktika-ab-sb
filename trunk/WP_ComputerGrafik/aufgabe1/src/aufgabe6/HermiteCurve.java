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


    private Vector3d p0,m0,m1, p1;

    public HermiteCurve(Vector3d p0, Vector3d m0, Vector3d m1, Vector3d p1) {
        this.p0 = p0;
        this.m0 = m0;
        this.m1 = m1;
        this.p1 = p1;
    }


    public static HermiteCurve create(Vector3d p0, Vector3d m0, Vector3d m1, Vector3d p1) {
        return new HermiteCurve(p0, m0, m1, p1);
    }

    private double h0(double t) {
        return Math.pow((1 - t), 2.0) * (1 + 2 * t);
    }
    private double h1(double t) {
        return Math.pow((1 - t), 2.0) *t;
    }
    private double h2(double t) {
        return Math.pow((1 - t), 2.0) * - Math.pow(t,2);
    }
    private double h3(double t) {
        return (3 - 2*t) * Math.pow(t,2);
    }

    private double h0_(double t) {
        return 6 * (t - 1) * t;
    }

    private double h1_(double t) {
        return 1 - 4 * t + 3 * t * t;
    }
    private  double h2_(double t) {
        return t * (3 * t - 2);
    }

    private double h3_(double t) {
        return -6 * (-1 + t) * t;
    }

    @Override
    public Vector3d eval(double value) {
      Vector3d p0 = new Vector3d(this.p0),
               m0 = new Vector3d(this.m0),
               m1 = new Vector3d(this.m1),
               p1 = new Vector3d(this.p1);

        p0.scale(h0(value));
        m0.scale(h1(value));
        m1.scale(h2(value));
        p1.scale(h3(value));

        p0.add(m0);
        p1.add(m1);
        p0.add(p1);

        return p0;
    }

    @Override
    public Vector3d derivative(double value) {
        Vector3d p0 = new Vector3d(this.p0),
                m0 = new Vector3d(this.m0),
                m1 = new Vector3d(this.m1),
                p1 = new Vector3d(this.p1);

        p0.scale(h0_(value));
        m0.scale(h1_(value));
        m1.scale(h2_(value));
        p1.scale(h3_(value));

        p0.add(m0);
        p1.add(m1);
        p0.add(p1);

        return p0;
    }

    @Override
    public Vector3d[] getControllPoints() {
        return new Vector3d[]{p0,m0,m1,p1};  //To change body of implemented methods use File | Settings | File Templates.
    }

}

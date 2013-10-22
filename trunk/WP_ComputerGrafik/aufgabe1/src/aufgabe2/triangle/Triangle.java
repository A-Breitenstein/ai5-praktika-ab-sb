package aufgabe2.triangle;

import javax.vecmath.Point3d;
import javax.vecmath.Vector3d;
import javax.vecmath.Vector3f;

/**
 * Created with IntelliJ IDEA.
 * User: abg667
 * Date: 08.10.13
 * Time: 12:52
 * To change this template use File | Settings | File Templates.
 */
public class Triangle {
  public int a,b,c;
  public Vector3f normal;

    public Triangle(int a, int b, int c) {
        this.a = a;
        this.b = b;
        this.c = c;
        normal = new Vector3f();
    }

    public static Triangle create(int a, int b, int c) {
        return new Triangle(a, b, c);
    }

    public void computeNormal(Point3d pA, Point3d pB, Point3d pC) {
        Point3d p1,p2,p3;
        p2 = new Point3d(pB);
        p3 = new Point3d(pC);
        p2.sub(pA);
        p3.sub(pA);

        Vector3f U = new Vector3f(p2);
        Vector3f V = new Vector3f(p3);
        normal.cross(U,V);
        normal.normalize();
    }
}

package aufgabe2.triangle;

import javax.vecmath.Point3d;
import javax.vecmath.TexCoord3f;
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
    public int a, b, c;
    public Vector3f normal;

    public int normala;
    public int normalb;
    public int normalc;

    public int texCoordA, texCoordB, texCoordC;

    public Triangle(int a, int b, int c) {
        this.a = a;
        this.b = b;
        this.c = c;
        normal = new Vector3f();
    }

    public static Triangle create(int a, int b, int c) {
        return new Triangle(a, b, c);
    }

    public void setTextureCoordinates(int a, int b, int c) {
        texCoordA = a;
        texCoordB = b;
        texCoordC = c;
    }

    public int getTexCoordA() {
        return texCoordA;
    }

    public int getTexCoordB() {
        return texCoordB;
    }

    public int getTexCoordC() {
        return texCoordC;
    }

    public void computeNormal(Point3d pA, Point3d pB, Point3d pC) {
        Point3d p1, p2, p3;
        p2 = new Point3d(pB);
        p3 = new Point3d(pC);
        p2.sub(pA);
        p3.sub(pA);

        Vector3f U = new Vector3f(p2);
        Vector3f V = new Vector3f(p3);
        normal.cross(U, V);
        normal.normalize();
    }

    public void setNormals(int normala, int normalb, int normalc) {
        this.normala = normala;
        this.normalb = normalb;
        this.normalc = normalc;
    }
}

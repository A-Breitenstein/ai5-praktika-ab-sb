package utils;

import aufgabe4.HalfEdge;
import aufgabe4.HalfEdgeVertex;

import javax.media.j3d.Transform3D;
import javax.vecmath.Matrix4d;
import javax.vecmath.Point3d;
import javax.vecmath.Point3f;
import javax.vecmath.Vector3f;

/**
 * Date: 05.10.13
 * Time: 02:44
 */
public class CGkursUtils{

    public static double dPI = Math.PI;
    public static double dPI_INVERSE = Math.PI;

    public static double d180 = 180.d;
    public static double d180_INVERSE = 1.d / 180.d;

    private static Vector3f zeroVector3f = new Vector3f(0, 0, 0);

    public static Transform3D rotateY(double angleInDegree, Transform3D transform3D) {

        Vector3f pos = new Vector3f();
        transform3D.get(pos);
        transform3D.setTranslation(zeroVector3f);
        transform3D.rotY(degreeToRadian(angleInDegree));
        transform3D.setTranslation(pos);

        return transform3D;
    }

    public static double radianToDegree(double radian) {
        return radian * d180 * dPI_INVERSE;
    }

    public static double degreeToRadian(double degree) {
        return degree * dPI * d180_INVERSE;
    }

    public static Transform3D rotateZ(double angleInDegree, Transform3D transform3D) {

        Vector3f pos = new Vector3f();
        transform3D.get(pos);
        transform3D.setTranslation(zeroVector3f);
        transform3D.rotZ(degreeToRadian(angleInDegree));
        transform3D.setTranslation(pos);

        return transform3D;
    }

    public static void rotateYMatrix4d(double angleInRadian, Transform3D transform3D) {
        double cos_a=Math.cos(angleInRadian);
        double sin_a=Math.sin(angleInRadian);

        Matrix4d m4_rotationYmitTranslation =  new Matrix4d(
                                                            cos_a,      0,  sin_a,  0,
                                                            0,          1,  0,      0,
                                                            - sin_a,    0,  cos_a,  0,
                                                            0,          0,  0,      1
                                                            );
        Matrix4d m4_old = new Matrix4d();
        transform3D.get(m4_old);
        m4_rotationYmitTranslation.mul(m4_old);
        transform3D.set(m4_rotationYmitTranslation);
    }


    public static boolean isEqual(Point3d p1,Point3d p2) {
//        final double delta = 0.0005;
        final double delta = 0.0;
        return assertDelta(p1.x,p2.x,delta) && assertDelta(p1.y,p2.y,delta) && assertDelta(p1.z,p2.z,delta);
    }

    public static boolean isEqual(Point3f p1,Point3f p2) {
//        final double delta = 0.0005;
        final double delta = 0.0;
        return assertDelta(p1.x,p2.x,delta) && assertDelta(p1.y,p2.y,delta) && assertDelta(p1.z,p2.z,delta);
    }
    private static boolean assertDelta(double d1, double d2, double delta) {
        final double actualDelta = (d1 > d2) ? (d1 - d2) : (d2 - d1);
        return (actualDelta <= delta);
    }

    public static boolean isEqual(HalfEdge startEdge, HalfEdge tempEdge) {
       return isEqual(startEdge.getVertex().getPosition(),tempEdge.getVertex().getPosition()) &&
              isEqual(startEdge.getNext().getVertex().getPosition(),tempEdge.getNext().getVertex().getPosition());
    }
}

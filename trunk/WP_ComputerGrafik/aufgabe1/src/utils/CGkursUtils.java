package utils;

import javax.media.j3d.Transform3D;
import javax.vecmath.Matrix4d;
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
}

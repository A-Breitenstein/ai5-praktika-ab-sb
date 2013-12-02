package aufgabe5.mappings;

import aufgabe2.triangle.ITriangleMesh;
import aufgabe2.triangle.Triangle;

import javax.vecmath.Point3d;
import javax.vecmath.TexCoord3f;

/**
 * Created with IntelliJ IDEA.
 * User: abg667
 * Date: 02.12.13
 * Time: 14:18
 * To change this template use File | Settings | File Templates.
 */
public class KugelTextureMapping extends TextureMapper{


    private KugelTextureMapping() {
    }

    public static KugelTextureMapping create() {
        return new KugelTextureMapping();
    }

    @Override
    public void mappTextureCoordinates(ITriangleMesh triangleMesh) {

        double[] bb = boundingBox(triangleMesh);
        double x_s = bb[3] - 0.5 * (Math.abs(bb[0]) + bb[3]),
                y_s = bb[4] - 0.5 * (Math.abs(bb[1]) + bb[4]),
                z_s = bb[5] - 0.5 * (Math.abs(bb[2]) + bb[5]);
        float uA, uB, uC,
                vA, vB, vC;

        int indexA, indexB, indexC;

        Triangle triangle;

        Point3d coordA, coordB, coordC;

        for (int i = 0; i < triangleMesh.getNumberOfVertices(); i++) {

            triangle = triangleMesh.getTriangle(i);

            coordA = triangleMesh.getVertex(triangle.a);
            coordB = triangleMesh.getVertex(triangle.b);
            coordC = triangleMesh.getVertex(triangle.c);

            uA = (float) u_kugelmapping(coordA.y, y_s, coordA.x, x_s);
            uB = (float) u_kugelmapping(coordB.y, y_s, coordB.x, x_s);
            uC = (float) u_kugelmapping(coordC.y, y_s, coordC.x, x_s);

            vA = (float) v_kugelmapping(coordA.y, y_s, coordA.x, x_s, coordA.z, z_s);
            vB = (float) v_kugelmapping(coordB.y, y_s, coordB.x, x_s, coordB.z, z_s);
            vC = (float) v_kugelmapping(coordC.y, y_s, coordC.x, x_s, coordC.z, z_s);

            indexA = triangleMesh.addTexCoord(new TexCoord3f(uA, vA, 0.f));
            indexB = triangleMesh.addTexCoord(new TexCoord3f(uB, vB, 0.f));
            indexC = triangleMesh.addTexCoord(new TexCoord3f(uC, vC, 0.f));

            triangle.setTextureCoordinates(indexA, indexB, indexC);
        }

    }

    private static double u_kugelmapping(double y, double y_s, double x, double x_s) {
        return (Math.PI + Math.atan2(y - y_s, x - x_s)) / 2 * Math.PI;
    }

    private static double v_kugelmapping(double y, double y_s, double x, double x_s, double z, double z_s) {
        return (Math.atan2(2 * Math.PI / (Math.sqrt(Math.pow(x - x_s, 2.f) + Math.pow(y - y_s, 2.f))), z - z_s)) / Math.PI;
    }

}

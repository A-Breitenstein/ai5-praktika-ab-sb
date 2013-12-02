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



        Point3d vertex;
        for (int i = 0; i < triangleMesh.getNumberOfVertices(); i++) {

            vertex = triangleMesh.getVertex(i);

            uA = (float) u_kugelmapping(vertex.y, y_s, vertex.x, x_s);
            vA = (float) v_kugelmapping(vertex.y, y_s, vertex.x, x_s, vertex.z, z_s);
            triangleMesh.addTexCoord(new TexCoord3f(uA, vA, 0.f));

        }

    }

    private static double u_kugelmapping(double y, double y_s, double x, double x_s) {
        return (Math.PI + Math.atan2(y - y_s, x - x_s)) / (2 * Math.PI);
    }

    private static double v_kugelmapping(double y, double y_s, double x, double x_s, double z, double z_s) {
        return (Math.atan2((Math.sqrt(Math.pow(x - x_s, 2.f) + Math.pow(y - y_s, 2.f))), z - z_s)) / Math.PI;
    }

}

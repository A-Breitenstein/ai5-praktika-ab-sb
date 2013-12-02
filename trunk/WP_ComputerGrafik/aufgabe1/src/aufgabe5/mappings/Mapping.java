package aufgabe5.mappings;

import aufgabe2.triangle.ITriangleMesh;
import aufgabe2.triangle.Triangle;

import javax.vecmath.Point3d;

/**
 * Created with IntelliJ IDEA.
 * User: abg667
 * Date: 02.12.13
 * Time: 14:18
 * To change this template use File | Settings | File Templates.
 */
public class Mapping {

    public static void uv_kugelMapping(ITriangleMesh triangleMesh) {

        double[] bb = boundingBox(triangleMesh);
        double x_s = bb[3] - 0.5 * (Math.abs(bb[0]) + bb[3]),
                y_s = bb[4] - 0.5 * (Math.abs(bb[1]) + bb[4]),
                z_s = bb[5] - 0.5 * (Math.abs(bb[2]) + bb[5]);

        double u, v, x, y, z;
        Triangle triangle;
        Point3d coordA,
                coordB,
                coordC;

        for (int i = 0; i < triangleMesh.getNumberOfVertices(); i++) {

            triangle = triangleMesh.getTriangle(i);

            float uA, uB, uC,
                    vA, vB, vC;

            coordA = triangleMesh.getVertex(triangle.a);
            coordB = triangleMesh.getVertex(triangle.b);
            coordC = triangleMesh.getVertex(triangle.c);

            uA = (float) u_kugelmapping(coordA.y, y_s, coordA.x, x_s);
            uB = (float) u_kugelmapping(coordB.y, y_s, coordB.x, x_s);
            uC = (float) u_kugelmapping(coordC.y, y_s, coordC.x, x_s);

            vA = (float) v_kugelmapping(coordA.y, y_s, coordA.x, x_s, coordA.z, z_s);
            vB = (float) v_kugelmapping(coordB.y, y_s, coordB.x, x_s, coordB.z, z_s);
            vC = (float) v_kugelmapping(coordC.y, y_s, coordC.x, x_s, coordC.z, z_s);
        }

    }

    private static double u_kugelmapping(double y, double y_s, double x, double x_s) {
        return (Math.PI + Math.atan2(y - y_s, x - x_s)) / 2 * Math.PI;
    }

    private static double v_kugelmapping(double y, double y_s, double x, double x_s, double z, double z_s) {
        return (Math.atan2(2 * Math.PI / (Math.sqrt(Math.pow(x - x_s, 2.f) + Math.pow(y - y_s, 2.f))), z - z_s)) / Math.PI;
    }

    /**
     * @param triangleMesh
     * @return double[]{bb_x_low[0], bb_y_low[1], bb_z_low[2], bb_x_max[3], bb_y_max[4], bb_z_max[5]}
     */
    public static double[] boundingBox(ITriangleMesh triangleMesh) {
        double bb_x_low = 0.f, bb_y_low = 0.f, bb_z_low = 0.f, bb_x_max = 0.f, bb_y_max = 0.f, bb_z_max = 0.0f;

        Point3d vertex;
        for (int i = 0; i < triangleMesh.getNumberOfVertices(); i++) {
            vertex = triangleMesh.getVertex(i);

            if (vertex.x <= bb_x_low) {
                bb_x_low = vertex.x;
            } else if (vertex.x >= bb_x_max) {
                bb_x_max = vertex.x;
            }

            if (vertex.y <= bb_y_low) {
                bb_y_low = vertex.y;
            } else if (vertex.y >= bb_y_max) {
                bb_y_max = vertex.y;
            }

            if (vertex.z <= bb_z_low) {
                bb_z_low = vertex.z;
            } else if (vertex.z >= bb_z_max) {
                bb_z_max = vertex.z;
            }
        }

        return new double[]{bb_x_low, bb_y_low, bb_z_low, bb_x_max, bb_y_max, bb_z_max};
    }


}

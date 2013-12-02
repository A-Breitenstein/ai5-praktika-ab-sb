package aufgabe5.mappings;

import aufgabe2.triangle.ITriangleMesh;

import javax.vecmath.Point3d;

/**
 * Created with IntelliJ IDEA.
 * User: Alexander Breitenstein
 * Date: 02.12.13
 * Time: 18:40
 */
public abstract class TextureMapper {

    abstract public void mappTextureCoordinates(ITriangleMesh iTriangleMesh);

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

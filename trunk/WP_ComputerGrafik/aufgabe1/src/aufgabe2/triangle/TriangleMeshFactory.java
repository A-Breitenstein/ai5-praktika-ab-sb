/**
 * Prof. Philipp Jenke
 * Hochschule für Angewandte Wissenschaften (HAW), Hamburg
 * Lecture demo program.
 */
package aufgabe2.triangle;

import javax.vecmath.Point3d;
import javax.vecmath.TexCoord3f;

/**
 * Create example triangle meshes.
 * 
 * @author Philipp Jenke
 * 
 */
public class TriangleMeshFactory {

    /**
     * Create a simple triangle mesh consisting of a single triangle.
     * 
     * @param mesh
     */
    public static void createTriangle(ITriangleMesh mesh) {
        if (mesh == null) {
            return;
        }
        mesh.clear();

        // Corner points
        Point3d pA = new Point3d(0, 0, 0);
        Point3d pB = new Point3d(0.5, 0, 0);
        Point3d pC = new Point3d(0, 0.5, 0);

        // Add vertices to mesh
        mesh.addVertex(pA);
        mesh.addVertex(pB);
        mesh.addVertex(pC);

        // Create triangle, compute normal and add to mesh.
        Triangle t = Triangle.create(0, 1, 2);
        t.computeNormal(pA, pB, pC);
        mesh.addTriangle(t);
    }

    /**
     * Create a sphere surface mesh
     * 
     * @param mesh
     */
    public static void createSphere(ITriangleMesh mesh) {
        if (mesh == null) {
            return;
        }

        mesh.clear();

        int resolution = 20;
        double radius = 2;

        // Iterate horizontally.
        for (int x = 0; x < resolution; x++) {
            // Iterate vertically
            for (int y = 0; y < resolution; y++) {
                double phi = (float) x / (float) resolution * Math.PI / 180.0
                        * 360.0;
                double theta = (float) y / (float) resolution * Math.PI / 180.0
                        * 180.0;
                double phiPlus = (float) (x + 1) / (float) resolution * Math.PI
                        / 180.0 * 360.0;
                double thetaPlus = (float) (y + 1) / (float) resolution
                        * Math.PI / 180.0 * 180.0;

                Point3d p00 = new Point3d(radius * Math.sin(theta)
                        * Math.cos(phi), radius * Math.sin(theta)
                        * Math.sin(phi), radius * Math.cos(theta));
                Point3d p01 = new Point3d(radius * Math.sin(thetaPlus)
                        * Math.cos(phi), radius * Math.sin(thetaPlus)
                        * Math.sin(phi), radius * Math.cos(thetaPlus));
                Point3d p10 = new Point3d(radius * Math.sin(theta)
                        * Math.cos(phiPlus), radius * Math.sin(theta)
                        * Math.sin(phiPlus), radius * Math.cos(theta));
                Point3d p11 = new Point3d(radius * Math.sin(thetaPlus)
                        * Math.cos(phiPlus), radius * Math.sin(thetaPlus)
                        * Math.sin(phiPlus), radius * Math.cos(thetaPlus));

                int a = mesh.addVertex(p00);
                int b = mesh.addVertex(p01);
                int c = mesh.addVertex(p11);

                Triangle t = Triangle.create(a, b, c);

                t.computeNormal(p00, p01, p11);
                mesh.addTriangle(t);

                a = mesh.addVertex(p00);
                b = mesh.addVertex(p11);
                c = mesh.addVertex(p10);
                t = Triangle.create(a, b, c);

                t.computeNormal(p00, p11, p10);
                mesh.addTriangle(t);

            }
        }
    }
}

/**
 * Prof. Philipp Jenke
 * Hochschule für Angewandte Wissenschaften (HAW), Hamburg
 * Lecture demo program.
 */
package aufgabe6.grundgeruest;

import javax.media.j3d.TriangleArray;

/**
 * Some test geometry.
 * 
 * @author Philipp Jenke
 * 
 */
public class TestGeometry extends TriangleArray {

    /**
     * Constructor.
     */
    TestGeometry() {
        super(3, COORDINATES);
        double[] v0 = { -0.5, -0.5, -0.5 };
        double[] v1 = { 0.5, -0.5, -0.5 };
        double[] v2 = { 0.5, 0.5, 0.5 };
        setCoordinate(0, v0);
        setCoordinate(1, v1);
        setCoordinate(2, v2);
    }
    
    /*
     * // Create some test geometry
     * 
     * GeometryInfo geometryInfo = new GeometryInfo(new TestGeometry());
     * NormalGenerator ng = new NormalGenerator();
     * ng.generateNormals(geometryInfo); Shape3D shape = new
     * Shape3D(geometryInfo.getGeometryArray()); setColor(shape, new
     * Color3f(0.75f, 0.25f, 0.25f)); scene.addChild(shape);
     */
}

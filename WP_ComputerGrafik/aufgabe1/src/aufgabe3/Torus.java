/**
 * Prof. Philipp Jenke
 * Hochschule für Angewandte Wissenschaften (HAW), Hamburg
 * Lecture demo program.
 */
package aufgabe3;

import javax.vecmath.Point3d;

/**
 * Implicit function of a 3-dimensional torus.
 * 
 * @author Philipp Jenke
 * 
 */
public class Torus implements ImplicitFunction3D {

    /**
     * Outer radius of the torus.
     */
    private double radiusOuter;

    /**
     * Inner radius of the torus.
     */
    private double radiusInner;

    /**
     * Constructor.
     * 
     * @param radius
     *            Initial value for the radius.
     */
    public Torus(double radiusOuter, double radiusInner) {
        this.radiusOuter = radiusOuter;
        this.radiusInner = radiusInner;
    }

    /*
     * (nicht-Javadoc)
     * 
     * @see
     * edu.cg1.exercises.marchingcubes.ImplicitFunction3D#f(javax.vecmath.Point3d
     * )
     */
    @Override
    public double f(Point3d p) {
        return Math.pow((Math.pow(p.x, 2) + Math.pow(p.y, 2) + Math.pow(p.z, 2)
                + Math.pow(radiusOuter, 2) - Math.pow(radiusInner, 2)), 2)
                - 4.0
                * Math.pow(radiusOuter, 2)
                * (Math.pow(p.x, 2) + Math.pow(p.y, 2));
    }
}

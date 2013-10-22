/**
 * Prof. Philipp Jenke
 * Hochschule für Angewandte Wissenschaften (HAW), Hamburg
 * Lecture demo program.
 */
package aufgabe3;

import javax.vecmath.Point3d;

/**
 * Implicit function of a 3-dimensional sphere.
 * 
 * @author Philipp Jenke
 * 
 */
public class Sphere implements ImplicitFunction3D {

    /**
     * Radius of the sphere.
     */
    private double radius;

    /**
     * Constructor.
     * 
     * @param radius
     *            Initial value for the radius.
     */
    public Sphere(double radius) {
        this.radius = radius;
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
        return p.x * p.x + p.y * p.y + p.z * p.z - radius * radius;
    }

}

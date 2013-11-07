/**
 * Prof. Philipp Jenke
 * Hochschule für Angewandte Wissenschaften (HAW), Hamburg
 * Lecture demo program.
 */
package aufgabe3;

import javax.vecmath.Point3d;

/**
 * Parent interface for implicit functions.
 * 
 * @author Philipp Jenke
 * 
 */
public interface ImplicitFunction3D {
    /**
     * Evaluates the implicit function a given spatial location.
     * 
     * @param x
     *            The function is evaluated at this point.
     * @return Function value.
     */
    public double f(Point3d p);

}

package aufgabe6;

import javax.vecmath.Vector3d;

/**
 * Created with IntelliJ IDEA.
 * User: abg628
 * Date: 03.12.13
 * Time: 08:35
 * To change this template use File | Settings | File Templates.
 */
public interface Curve {

    Vector3d eval(double value);

    Vector3d derivative(double value);
}

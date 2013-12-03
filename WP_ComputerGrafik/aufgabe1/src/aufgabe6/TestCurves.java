package aufgabe6;

import javax.vecmath.Vector3d;

/**
 * Created with IntelliJ IDEA.
 * User: abg667
 * Date: 03.12.13
 * Time: 13:58
 * To change this template use File | Settings | File Templates.
 */
public class TestCurves {

    public static void main(String[] args) {
        Vector3d v1 = new Vector3d(1,2,3);
        Vector3d v2 = new Vector3d(4,4,3);
        Vector3d v3 = new Vector3d(9,2,5);
        Vector3d v4 = new Vector3d(6,7,3);

        Curve curve = MonomialCurve.create(v1, v2, v3, v4);
        System.out.println(curve.eval(4).equals(new Vector3d(545,498,287)));
        System.out.println(curve.derivative(4).equals(new Vector3d(364, 356, 187)));
    }
}

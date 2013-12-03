package aufgabe6;

import javax.media.j3d.*;
import javax.vecmath.Color3f;
import javax.vecmath.Point3d;
import javax.vecmath.Vector3d;
import java.awt.*;

/**
 * Created with IntelliJ IDEA.
 * User: abg667
 * Date: 03.12.13
 * Time: 14:55
 * To change this template use File | Settings | File Templates.
 */
public class Plotter {
    public static Shape3D plottFunction(Curve curve, int precision) {
        Point3d[] point3ds = new Point3d[2*precision];
        int index = 0;
        double step = 1.0 / (double) precision;
        double max = 1 - step;
        for (double t = 0; t < max; t += step,index+=2) {
            point3ds[index] = new Point3d(curve.eval(t));
            point3ds[index+1] = new Point3d(curve.eval(t+step));
        }

        Vector3d[] curveControllPoints = curve.getControllPoints();
        Point3d[] controllPoints = new Point3d[curveControllPoints.length*2];

        for (int i = 0,x = 0; i < curveControllPoints.length; x++,i+=2) {
            controllPoints[i] = new Point3d(curveControllPoints[x]);
            controllPoints[i + 1] = new Point3d(curveControllPoints[x + 1]);

        }

        LineArray lineArray_function = new LineArray(point3ds.length, GeometryArray.COORDINATES);
        lineArray_function.setCoordinates(0, point3ds);

        LineArray lineArray_controllPoints = new LineArray(controllPoints.length, GeometryArray.COORDINATES);
        lineArray_controllPoints.setCoordinates(0, controllPoints);


        Appearance lineApp_function = new Appearance();
        LineAttributes lineAttr = new LineAttributes(5, LineAttributes.PATTERN_DASH_DOT, true);
        lineApp_function.setLineAttributes(lineAttr);

        ColoringAttributes ca = new ColoringAttributes(new Color3f(new Color(50, 87, 128)),ColoringAttributes.SHADE_FLAT);
        lineApp_function.setColoringAttributes(ca);

        Appearance lineApp_controlls = new Appearance();
        LineAttributes lineAttr2 = new LineAttributes(5, LineAttributes.PATTERN_DASH_DOT, true);
        lineApp_controlls.setLineAttributes(lineAttr2);

        ColoringAttributes ca2 = new ColoringAttributes(new Color3f(new Color(50, 87, 128)),ColoringAttributes.SHADE_FLAT);
        lineApp_controlls.setColoringAttributes(ca2);



        Shape3D shape3D_function = new Shape3D(lineArray_function);
        Shape3D shape3D_controllpoints = new Shape3D(lineArray_controllPoints);

        shape3D_controllpoints.setAppearance(lineApp_controlls);
        shape3D_function.setAppearance(lineApp_function);

        return shape3D_function;
    }
}

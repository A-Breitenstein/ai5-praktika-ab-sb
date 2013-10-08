package aufgabe2.triangle;

import javax.vecmath.Point3d;
import javax.vecmath.Vector3d;
import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: abg667
 * Date: 08.10.13
 * Time: 12:51
 * To change this template use File | Settings | File Templates.
 */
public class TriangleMesh implements ITriangleMesh {
    List<Triangle> triangleList;
    List<Point3d> point3dList;

    private TriangleMesh() {
        triangleList = new ArrayList<Triangle>();
        point3dList = new ArrayList<Point3d>();
    }

    public static TriangleMesh create() {
        return new TriangleMesh();
    }


    @Override

    public void addTriangle(Triangle t) {
        triangleList.add(t);
    }

    @Override
    public int addVertex(Point3d v) {
        point3dList.add(v);
        return point3dList.size()-1;
    }

    @Override
    public int getNumberOfTriangles() {
        return triangleList.size();
    }

    @Override
    public int getNumberOfVertices() {
        return point3dList.size();
    }

    @Override
    public Triangle getTriangle(int index) {
        return triangleList.get(index);
    }

    @Override
    public Point3d getVertex(int index) {
        return point3dList.get(index);
    }

    @Override
    public void clear() {
        point3dList = new ArrayList<Point3d>();
        triangleList = new ArrayList<Triangle>();
    }
}

package aufgabe2.triangle;


import javax.vecmath.Point3d;
import javax.vecmath.Vector3d;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

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

    public TriangleMesh() {
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

    @Override
    public List<Triangle> getAllAdjacentTrianglesToVertex(int pointIndex) {
        List<Triangle> triangleList = new ArrayList<Triangle>();
        for (Triangle triangle : this.triangleList) {
            if(triangle.a == pointIndex)
                triangleList.add(triangle);
            else if(triangle.b == pointIndex)
                triangleList.add(triangle);
            else if(triangle.c == pointIndex)
                triangleList.add(triangle);
        }
        return triangleList;
    }

    @Override
    public void removeDuplicatedPointsAndFixTriangles() {
        int size = point3dList.size();
        Point3d currentPoint;
        List<Point3d> uniquePoints = new ArrayList<Point3d>();
        int index;
        for (int i = 0; i < size; i++) {
            currentPoint = point3dList.get(i);

            if (!containsPoint3d(uniquePoints, currentPoint)) {

                uniquePoints.add(currentPoint);
                index = uniquePoints.size()-1;
                for (Triangle triangle : triangleList) {

                    if(isEqual(point3dList.get(triangle.a),currentPoint))
                        triangle.a = index;
                    else if (isEqual(point3dList.get(triangle.b),currentPoint))
                        triangle.b = index;
                    else if(isEqual(point3dList.get(triangle.c),currentPoint))
                        triangle.c = index;
                }

            }
        }
        System.out.println("p3L:"+point3dList.size()+", uniqu:"+uniquePoints.size());

        if (containsPoint3d(uniquePoints, point3dList.get(4003))) {
            System.out.println("ist drin");
        }else System.out.println("ist nicht drin");

        point3dList = uniquePoints;

    }

    private boolean containsPoint3d(List<Point3d> uniquePoints, Point3d currentPoint) {
        for (Point3d uniquePoint : uniquePoints) {
            if(isEqual(uniquePoint,currentPoint))
                return true;
        }
        return false;
    }

    private boolean isEqual(Point3d p1,Point3d p2) {
//        final double delta = 0.0005;
        final double delta = 0.0;
        return assertDelta(p1.x,p2.x,delta) && assertDelta(p1.y,p2.y,delta) && assertDelta(p1.z,p2.z,delta);
    }

    private boolean assertDelta(double d1, double d2, double delta) {
        final double actualDelta = (d1 > d2) ? (d1 - d2) : (d2 - d1);
        return (actualDelta <= delta);
    }
}

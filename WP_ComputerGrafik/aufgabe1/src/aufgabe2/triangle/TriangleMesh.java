package aufgabe2.triangle;


import aufgabe4.*;

import javax.vecmath.Point3d;
import javax.vecmath.Point3f;
import javax.vecmath.TexCoord3f;
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
    List<TexCoord3f> texCoord3fList;


    public TriangleMesh() {
        triangleList = new ArrayList<Triangle>();
        point3dList = new ArrayList<Point3d>();
        texCoord3fList = new ArrayList<TexCoord3f>();
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
        return point3dList.size() - 1;
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
            if (triangle.a == pointIndex)
                triangleList.add(triangle);
            else if (triangle.b == pointIndex)
                triangleList.add(triangle);
            else if (triangle.c == pointIndex)
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
                index = uniquePoints.size() - 1;
                for (Triangle triangle : triangleList) {

                    if (isEqual(point3dList.get(triangle.a), currentPoint))
                        triangle.a = index;
                    else if (isEqual(point3dList.get(triangle.b), currentPoint))
                        triangle.b = index;
                    else if (isEqual(point3dList.get(triangle.c), currentPoint))
                        triangle.c = index;
                }

            }
        }
        System.out.println("p3L:" + point3dList.size() + ", uniqu:" + uniquePoints.size());

        if (containsPoint3d(uniquePoints, point3dList.get(4003))) {
            System.out.println("ist drin");
        } else System.out.println("ist nicht drin");

        point3dList = uniquePoints;
    }

    @Override
    public void removeDuplicatedTexturePointsAndFixTriangles() {
        int size = texCoord3fList.size();
        TexCoord3f currentPoint;
        ArrayList<TexCoord3f> uniquePoints = new ArrayList<TexCoord3f>();
        int index;
        for (int i = 0; i < size; i++) {
            currentPoint = texCoord3fList.get(i);

            if (!containsTexCoord3f(uniquePoints, currentPoint)) {

                uniquePoints.add(currentPoint);
                index = uniquePoints.size() - 1;
                for (Triangle triangle : triangleList) {

                    if (isEqual(texCoord3fList.get(triangle.texCoordA), currentPoint))
                        triangle.texCoordA = index;
                    else if (isEqual(texCoord3fList.get(triangle.texCoordB), currentPoint))
                        triangle.texCoordB = index;
                    else if (isEqual(texCoord3fList.get(triangle.texCoordC), currentPoint))
                        triangle.texCoordC = index;
                }

            }
        }
        System.out.println("t3L:" + texCoord3fList.size() + ", uniqu:" + uniquePoints.size());

        if (containsTexCoord3f(uniquePoints, texCoord3fList.get(4003))) {
            System.out.println("ist drin");
        } else System.out.println("ist nicht drin");

        texCoord3fList = uniquePoints;
    }

    private boolean containsTexCoord3f(List<TexCoord3f> uniqueTexPoints, TexCoord3f currentTexPoint) {
        for (TexCoord3f uniquePoint : uniqueTexPoints) {
            if (isEqual(uniquePoint, currentTexPoint))
                return true;
        }
        return false;
    }

    private boolean containsPoint3d(List<Point3d> uniquePoints, Point3d currentPoint) {
        for (Point3d uniquePoint : uniquePoints) {
            if (isEqual(uniquePoint, currentPoint))
                return true;
        }
        return false;
    }

    private boolean isEqual(TexCoord3f p1, TexCoord3f p2) {
//        final double delta = 0.0005;
        final double delta = 0.0;
        return assertDelta(p1.x, p2.x, delta) && assertDelta(p1.y, p2.y, delta) && assertDelta(p1.z, p2.z, delta);
    }

    private boolean isEqual(Point3d p1, Point3d p2) {
//        final double delta = 0.0005;
        final double delta = 0.0;
        return assertDelta(p1.x, p2.x, delta) && assertDelta(p1.y, p2.y, delta) && assertDelta(p1.z, p2.z, delta);
    }

    private boolean isEqual(Point3f p1, Point3f p2) {
//        final double delta = 0.0005;
        final double delta = 0.0;
        return assertDelta(p1.x, p2.x, delta) && assertDelta(p1.y, p2.y, delta) && assertDelta(p1.z, p2.z, delta);
    }

    private boolean assertDelta(double d1, double d2, double delta) {
        final double actualDelta = (d1 > d2) ? (d1 - d2) : (d2 - d1);
        return (actualDelta <= delta);
    }

    @Override
    public HalfEdgeDatastructure convertToHalfEdgeDatastructure() {
        HalfEdgeDatastructure hed = new HalfEdgeDatastructure();
        for (Triangle triangle : triangleList) {
            addTriangleToHalfEdgeDatasturcture(triangle, hed);
        }
        HalfEdge current, temp;
        HalfEdgeVertex current_source, current_target;
        for (int i = 0; i < hed.getNumberOfHalfEdges(); i++) {
            current = hed.getHalfEdge(i);
            current_source = current.getVertex();
            current_target = current.getNext().getVertex();
            for (int j = 1; j < hed.getNumberOfHalfEdges(); j++) {
                temp = hed.getHalfEdge(j);
                if (current_target.equals(temp.getVertex()) && current_source.equals(temp.getNext().getVertex())) {
                    current.setOpposite(temp);
                    temp.setOpposite(current);
                }
            }
        }
        return hed;
    }

    @Override
    public int addTexCoord(TexCoord3f texCoord3f) {
        texCoord3fList.add(texCoord3f);
        return getNumberOfTexCoord() - 1;
    }

    @Override
    public int getNumberOfTexCoord() {
        return texCoord3fList.size();
    }

    @Override
    public TexCoord3f getTexCoord(int index) {
        return texCoord3fList.get(index);
    }

    private void addTriangleToHalfEdgeDatasturcture(Triangle triangle, HalfEdgeDatastructure hed) {
        final HalfEdgeVertex hev_a;
        final HalfEdgeVertex hev_b;
        final HalfEdgeVertex hev_c;
        final HalfEdge he1;
        final HalfEdge he2;
        final HalfEdge he3;
        final HalfEdgeTriangle het;

        //Points
        Point3f pA = new Point3f(point3dList.get(triangle.a));
        Point3f pB = new Point3f(point3dList.get(triangle.b));
        Point3f pC = new Point3f(point3dList.get(triangle.c));
        Point3f temp;

        boolean newPoint_A = true, newPoint_B = true, newPoint_C = true;
//        if(hed.getNumberOfVertices() == 0){
//            newPoint_A = newPoint_B = newPoint_C = true;
//        }
        //bevor punkt erstellt wird, prüfen ob es den punkt bereits im HED gibt und diesen verwenden
        for (int i = 0; i < hed.getNumberOfVertices(); i++) {
            temp = hed.getVertex(i).getPosition();

            if (isEqual(temp, pA)) {
                pA = temp;
                newPoint_A = false;
            } else if (isEqual(temp, pB)) {
                pB = temp;
                newPoint_B = false;
            } else if (isEqual(temp, pC)) {
                pC = temp;
                newPoint_C = false;
            }

        }

        hev_a = new HalfEdgeVertex(pA);
        hev_b = new HalfEdgeVertex(pB);
        hev_c = new HalfEdgeVertex(pC);

        he1 = new HalfEdge();
        he2 = new HalfEdge();
        he3 = new HalfEdge();
        het = new HalfEdgeTriangle();


        if (newPoint_A) {
            hed.addVertex(hev_a);
            hev_a.setHalfEdge(he1);
        }
        he1.setVertex(hev_a);

        if (newPoint_B) {
            hed.addVertex(hev_b);
            hev_b.setHalfEdge(he2);
        }
        he2.setVertex(hev_b);

        if (newPoint_C) {
            hed.addVertex(hev_c);
            hev_c.setHalfEdge(he3);
        }
        he3.setVertex(hev_c);


        he1.setNext(he2);
        he2.setNext(he3);
        he3.setNext(he1);

        he1.setPrev(he3);
        he3.setPrev(he2);
        he2.setPrev(he1);

        he1.setFacet(het);
        he2.setFacet(het);
        he3.setFacet(het);

        het.setHalfEdge(he1);

        hed.addHalfEdge(he1);
        hed.addHalfEdge(he2);
        hed.addHalfEdge(he3);

        hed.addFacet(het);


    }
}

package aufgabe2;

import aufgabe2.triangle.ITriangleMesh;
import aufgabe2.triangle.Triangle;

import javax.media.j3d.Geometry;
import javax.media.j3d.GeometryArray;
import javax.media.j3d.Shape3D;
import javax.media.j3d.TriangleArray;
import javax.vecmath.Point3d;
import javax.vecmath.Vector3f;

/**
 * Created with IntelliJ IDEA.
 * User: abg667
 * Date: 08.10.13
 * Time: 13:53
 * To change this template use File | Settings | File Templates.
 */
public class MeshShapeFactory {
    public static Shape3D createMeshShape(ITriangleMesh mesh) {
        Shape3D shape = new Shape3D();
        TriangleArray triangleArray = new TriangleArray(mesh.getNumberOfTriangles()*3, GeometryArray.COORDINATES | GeometryArray.NORMALS);

        for (int triangleIndex = 0,coordinateIndex = 0; triangleIndex < mesh.getNumberOfTriangles(); triangleIndex++,coordinateIndex+=3) {
            Triangle triangle = mesh.getTriangle(triangleIndex);
            triangleArray.setCoordinates(coordinateIndex,new Point3d[]{mesh.getVertex(triangle.a),mesh.getVertex(triangle.b),mesh.getVertex(triangle.c)});
            triangleArray.setNormals(coordinateIndex, new Vector3f[]{triangle.normal,triangle.normal,triangle.normal});

            /*
            triangleArray.setCoordinate(coordinateIndex,mesh.getVertex(triangle.a));
            triangleArray.setCoordinate(coordinateIndex+1,mesh.getVertex(triangle.b));
            triangleArray.setCoordinate(coordinateIndex+2,mesh.getVertex(triangle.c));
            triangleArray.setNormal(coordinateIndex,triangle.normal);
            triangleArray.setNormal(coordinateIndex+1,triangle.normal);
            triangleArray.setNormal(coordinateIndex+2,triangle.normal);
            */
        }

        shape.setGeometry(triangleArray);
        return shape;
    }
}

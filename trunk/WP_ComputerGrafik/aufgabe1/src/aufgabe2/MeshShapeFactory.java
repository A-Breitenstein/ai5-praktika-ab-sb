package aufgabe2;

import aufgabe2.triangle.ITriangleMesh;
import aufgabe2.triangle.Triangle;

import javax.media.j3d.Geometry;
import javax.media.j3d.GeometryArray;
import javax.media.j3d.Shape3D;
import javax.media.j3d.TriangleArray;
import javax.vecmath.Point3d;
import javax.vecmath.TexCoord3f;
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
        TriangleArray triangleArray = new TriangleArray(mesh.getNumberOfTriangles()*3, GeometryArray.COORDINATES | GeometryArray.NORMALS | GeometryArray.TEXTURE_COORDINATE_3);


        for (int triangleIndex = 0,coordinateIndex = 0; triangleIndex < mesh.getNumberOfTriangles(); triangleIndex++,coordinateIndex+=3) {
            Triangle triangle = mesh.getTriangle(triangleIndex);
            triangleArray.setCoordinates(coordinateIndex,new Point3d[]{mesh.getVertex(triangle.a),mesh.getVertex(triangle.b),mesh.getVertex(triangle.c)});
            triangleArray.setNormals(coordinateIndex, new Vector3f[]{triangle.normal,triangle.normal,triangle.normal});

            triangleArray.setTextureCoordinates(coordinateIndex,
                                                0,
                                                new TexCoord3f[]
                                                        {
                                                        mesh.getTexCoord(triangle.getTexCoordA()),
                                                        mesh.getTexCoord(triangle.getTexCoordB()),
                                                        mesh.getTexCoord(triangle.getTexCoordC())
                                                        }
            );
// triangleArray.setNormals(coordinateIndex, new Vector3f[]{n(mesh.getVertex(triangle.a)),n(mesh.getVertex(triangle.b)),n(mesh.getVertex(triangle.c))});
        }

        shape.setGeometry(triangleArray);
        return shape;
    }
    private static Vector3f n(Point3d p) {
       Vector3f tmp =  new Vector3f(p);
        tmp.normalize();
        return tmp;
    }
}

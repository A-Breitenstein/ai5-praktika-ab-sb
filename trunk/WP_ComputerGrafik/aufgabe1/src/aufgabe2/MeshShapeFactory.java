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

        Point3d[] vertexCoords = new Point3d[mesh.getNumberOfTriangles() * 3];
        TexCoord3f[] texCoords = new TexCoord3f[mesh.getNumberOfTriangles() * 3];
        Vector3f[] normals = new Vector3f[mesh.getNumberOfTriangles() * 3];

        for (int triangleIndex = 0,coordinateIndex = 0; triangleIndex < mesh.getNumberOfTriangles(); triangleIndex++,coordinateIndex+=3) {
            Triangle triangle = mesh.getTriangle(triangleIndex);

            vertexCoords[coordinateIndex] = mesh.getVertex(triangle.a);
            vertexCoords[coordinateIndex+1] =  mesh.getVertex(triangle.b);
            vertexCoords[coordinateIndex+2] =  mesh.getVertex(triangle.c);

            if (mesh.getNumberOfVertexNormals() == 0) {
                //TODO: vertexnormalen mitteln richtig implementieren...
                triangle.computeNormal(vertexCoords[coordinateIndex],vertexCoords[coordinateIndex+1],vertexCoords[coordinateIndex+2]);
                normals[coordinateIndex] = triangle.normal;
                normals[coordinateIndex+1] = triangle.normal;
                normals[coordinateIndex+2] = triangle.normal;
//                normals[coordinateIndex] = n(mesh.getVertex(triangle.a));
//                normals[coordinateIndex+1] = n(mesh.getVertex(triangle.b));
//                normals[coordinateIndex+2] = n(mesh.getVertex(triangle.c));


            }else {
                normals[coordinateIndex] = mesh.getVertexNormal(triangle.normala);
                normals[coordinateIndex+1] = mesh.getVertexNormal(triangle.normalb);
                normals[coordinateIndex+2] =mesh.getVertexNormal(triangle.normalc);
            }


            if (mesh.getNumberOfTexCoord() > 0) {
                texCoords[coordinateIndex] = mesh.getTexCoord(triangle.texCoordA);
                texCoords[coordinateIndex+1] = mesh.getTexCoord(triangle.texCoordB);
                texCoords[coordinateIndex+2] = mesh.getTexCoord(triangle.texCoordC);
            }

//            normals[coordinateIndex] = n(mesh.getVertex(triangle.a));
//            normals[coordinateIndex+1] = n(mesh.getVertex(triangle.b));
//            normals[coordinateIndex+2] = n(mesh.getVertex(triangle.c));
        }

        triangleArray.setCoordinates(0, vertexCoords);
        triangleArray.setNormals(0, normals);
        if(mesh.getNumberOfTexCoord() > 0)
            triangleArray.setTextureCoordinates(0,0,texCoords);

        shape.setGeometry(triangleArray);
        return shape;
    }
    private static Vector3f n(Point3d p) {
       Vector3f tmp =  new Vector3f(p);
        tmp.normalize();
        return tmp;
    }
}

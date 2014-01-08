package aufgabe2;

import aufgabe2.triangle.ITriangleMesh;
import aufgabe2.triangle.Triangle;
import aufgabe4.*;

import javax.media.j3d.Geometry;
import javax.media.j3d.GeometryArray;
import javax.media.j3d.Shape3D;
import javax.media.j3d.TriangleArray;
import javax.vecmath.Point3d;
import javax.vecmath.Point3f;
import javax.vecmath.TexCoord3f;
import javax.vecmath.Vector3f;
import java.util.ArrayList;
import java.util.List;

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
        IHalfEdgeDatastructure halfEdgeDatastructure;

        TriangleArray triangleArray = new TriangleArray(mesh.getNumberOfTriangles() * 3, GeometryArray.COORDINATES | GeometryArray.NORMALS | GeometryArray.TEXTURE_COORDINATE_3);

        Point3d[] vertexCoords = new Point3d[mesh.getNumberOfTriangles() * 3];
        TexCoord3f[] texCoords = new TexCoord3f[mesh.getNumberOfTriangles() * 3];
        Vector3f[] normals = new Vector3f[mesh.getNumberOfTriangles() * 3];

        if (mesh.getNumberOfVertexNormals() == 0) {
            mesh.removeDuplicatedPointsAndFixTriangles();
            mesh.removeDuplicatedTexturePointsAndFixTriangles();
            halfEdgeDatastructure = mesh.convertToHalfEdgeDatastructure();
            fillArraysWithHalfEdgeDatastructure(halfEdgeDatastructure,vertexCoords, texCoords, normals);
        }else {
            fillArraysWithTrianlgeMesh(mesh,vertexCoords, texCoords, normals);
        }

        triangleArray.setCoordinates(0, vertexCoords);
        triangleArray.setNormals(0, normals);
        if (mesh.getNumberOfTexCoord() > 0)
            triangleArray.setTextureCoordinates(0, 0, texCoords);

        shape.setGeometry(triangleArray);
        return shape;
    }

    private static void fillArraysWithTrianlgeMesh(ITriangleMesh mesh, Point3d[] vertexCoords, TexCoord3f[] texCoords, Vector3f[] normals) {
        for (int triangleIndex = 0, coordinateIndex = 0; triangleIndex < mesh.getNumberOfTriangles(); triangleIndex++, coordinateIndex += 3) {
            Triangle triangle = mesh.getTriangle(triangleIndex);

            vertexCoords[coordinateIndex] = mesh.getVertex(triangle.a);
            vertexCoords[coordinateIndex + 1] = mesh.getVertex(triangle.b);
            vertexCoords[coordinateIndex + 2] = mesh.getVertex(triangle.c);

            normals[coordinateIndex] = mesh.getVertexNormal(triangle.normala);
            normals[coordinateIndex + 1] = mesh.getVertexNormal(triangle.normalb);
            normals[coordinateIndex + 2] = mesh.getVertexNormal(triangle.normalc);
            
            texCoords[coordinateIndex] = mesh.getTexCoord(triangle.texCoordA);
            texCoords[coordinateIndex + 1] = mesh.getTexCoord(triangle.texCoordB);
            texCoords[coordinateIndex + 2] = mesh.getTexCoord(triangle.texCoordC);
        }
    }

    private static void fillArraysWithHalfEdgeDatastructure(IHalfEdgeDatastructure halfEdgeDatastructure, Point3d[] vertexCoords, TexCoord3f[] texCoords, Vector3f[] normals) {
        IHalfEdgeDatastructureOperations ohed = HalfEdgeDatastructureOperations.create(halfEdgeDatastructure);
        for (int triangleIndex = 0, coordinateIndex = 0; triangleIndex < ohed.getNumberOfFacets(); triangleIndex++, coordinateIndex += 3) {
            IHalfEdgeFacet triangle = ohed.getFacet(triangleIndex);
            HalfEdge tmpEdge;
            tmpEdge = triangle.getHalfEdge();
            HalfEdgeVertex tmpVertex;
            for (int i = 0; i < 3; i++) {
                tmpVertex = tmpEdge.getVertex();
                vertexCoords[coordinateIndex + i] = new Point3d(tmpVertex.getPosition());
                texCoords[coordinateIndex + i] = tmpVertex.getTexCoord();
//                normals[coordinateIndex + i] = calcNormal(ohed, tmpVertex);
                normals[coordinateIndex + i] = n(new Point3d(tmpVertex.getPosition()));
                tmpEdge = tmpEdge.getNext();
            }
        }
    }
    private static Vector3f calcNormal(IHalfEdgeDatastructureOperations ohed,HalfEdgeVertex halfEdgeVertex) {
        Point3f p1,p2,p3;
        Vector3f v1, v2,normal,normalAccu = new Vector3f();
        HalfEdge tmpEdge;

        for (IHalfEdgeFacet iHalfEdgeFacet : ohed.getIncidentFacets(halfEdgeVertex)) {
            tmpEdge = iHalfEdgeFacet.getHalfEdge();
            p1 = tmpEdge.getVertex().getPosition();
            tmpEdge = tmpEdge.getNext();
            p2 = new Point3f(tmpEdge.getVertex().getPosition());
            tmpEdge = tmpEdge.getNext();
            p3 = new Point3f(tmpEdge.getVertex().getPosition());

            p2.sub(p1);
            p3.sub(p1);

            v1 = new Vector3f(p2);
            v2 = new Vector3f(p3);
            normal = new Vector3f();
            normal.cross(v1, v2);

            normalAccu.add(normal);
        }
        normalAccu.normalize();
        return normalAccu;


            /*
            Point3d p1, p2, p3;
        p2 = new Point3d(pB);
        p3 = new Point3d(pC);
        p2.sub(pA);
        p3.sub(pA);

        Vector3f U = new Vector3f(p2);
        Vector3f V = new Vector3f(p3);
        normal.cross(U, V);
        normal.normalize();*/

    }
    private static Vector3f n(Point3d p) {
       Vector3f tmp =  new Vector3f(p);
        tmp.normalize();
        return tmp;
    }




}

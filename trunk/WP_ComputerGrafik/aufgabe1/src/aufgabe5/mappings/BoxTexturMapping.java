package aufgabe5.mappings;

import aufgabe2.triangle.ITriangleMesh;
import aufgabe2.triangle.Triangle;

import javax.vecmath.Point3d;
import javax.vecmath.TexCoord3f;

/**
 * Created with IntelliJ IDEA.
 * User: Alexander Breitenstein
 * Date: 02.12.13
 * Time: 20:46
 */
public class BoxTexturMapping extends TextureMapper {


    private BoxTexturMapping() {
    }

    public static BoxTexturMapping create() {
        return new BoxTexturMapping();
    }

    @Override
    public void mappTextureCoordinates(ITriangleMesh iTriangleMesh) {
        double[] bb = boundingBox(iTriangleMesh);

        float uA, uB, uC,
                vA, vB, vC;

        int indexA, indexB, indexC;

        Triangle triangle;

        Point3d coordA, coordB, coordC;

        for (int i = 0; i < iTriangleMesh.getNumberOfTriangles(); i++) {

            triangle = iTriangleMesh.getTriangle(i);

            coordA = iTriangleMesh.getVertex(triangle.a);
            coordB = iTriangleMesh.getVertex(triangle.b);
            coordC = iTriangleMesh.getVertex(triangle.c);

            uA = (float) uv_boxmapping(coordA.x,bb[3],bb[0]);
            uB = (float) uv_boxmapping(coordB.x, bb[3], bb[0]);
            uC = (float) uv_boxmapping(coordC.x, bb[3], bb[0]);

            vA = (float) uv_boxmapping(coordA.y, bb[4], bb[1]);
            vB = (float) uv_boxmapping(coordB.y, bb[4], bb[1]);
            vC = (float) uv_boxmapping(coordC.y, bb[4], bb[1]);

            indexA = iTriangleMesh.addTexCoord(new TexCoord3f(uA, vA, 0.f));
            indexB = iTriangleMesh.addTexCoord(new TexCoord3f(uB, vB, 0.f));
            indexC = iTriangleMesh.addTexCoord(new TexCoord3f(uC, vC, 0.f));

            triangle.setTextureCoordinates(indexA, indexB, indexC);
    }



}
    private static double uv_boxmapping(double value, double bb_max, double bb_low) {
        return((value - bb_low) / (bb_max - bb_low));
    }
}
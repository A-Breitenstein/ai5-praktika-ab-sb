package aufgabe6;

import javax.vecmath.Vector3d;

/**
 * Created with IntelliJ IDEA.
 * User: abg667
 * Date: 05.12.13
 * Time: 10:02
 * To change this template use File | Settings | File Templates.
 */
public class VectorRichtig {

    public double x, y , z;

    private VectorRichtig(double x, double y, double z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    public static VectorRichtig create(double x, double y, double z) {
        return new VectorRichtig(x, y, z);
    }

    public static VectorRichtig create(Vector3d vector3d) {
        return new VectorRichtig(vector3d.x, vector3d.y, vector3d.z);
    }

    public static VectorRichtig create(VectorRichtig vectorRichtig) {
        return new VectorRichtig(vectorRichtig.x, vectorRichtig.y, vectorRichtig.z);
    }

    public VectorRichtig add(VectorRichtig vectorRichtig) {
        return VectorRichtig.create(vectorRichtig.x+x,vectorRichtig.y+y,vectorRichtig.z+z);
    }

    public VectorRichtig sub(VectorRichtig vectorRichtig) {
        return VectorRichtig.create(x-vectorRichtig.x,y-vectorRichtig.y,z-vectorRichtig.z);
    }

    public VectorRichtig scale(double scalar) {
        return VectorRichtig.create(x*scalar,y*scalar,z*scalar);
    }

    public Vector3d toVektorFalsch() {
        return new Vector3d(x, y, z);
    }

    @Override
    public String toString() {
        return "VectorRichtig{" +
                "x=" + x +
                ", y=" + y +
                ", z=" + z +
                '}';
    }
}

/**
 * Prof. Philipp Jenke
 * Hochschule für Angewandte Wissenschaften (HAW), Hamburg
 * Lecture demo program.
 */
package aufgabe4;

import javax.vecmath.Point3f;
import javax.vecmath.TexCoord3f;

/**
 * Representation of a vertex in the half edge date structure.
 * 
 * @author Philipp Jenke
 * 
 */
public class HalfEdgeVertex {

    /**
     * Name, used for debugging purposes.
     */
    private String name;

    /**
     * Position in 3-space
     */
    private final Point3f position;

    private  TexCoord3f texCoord;

    public TexCoord3f getTexCoord() {
        return texCoord;
    }

    public void setTexCoord(TexCoord3f texCoord) {
        this.texCoord = texCoord;
    }

    /**
     * Reference to an adjacent half edge
     */
    private HalfEdge halfEdge = null;

    /**
     * Constructor
     *
     * @param position
     *            Position of the vertex.
     * @param texCoord
     */
    public HalfEdgeVertex(Point3f position) {
        this.position = position;
    }
    public HalfEdgeVertex(Point3f position, TexCoord3f texCoord) {
        this.position = position;
        this.texCoord = texCoord;
    }

    /**
     * Getter
     */
    public HalfEdge getHalfEdge() {
        return halfEdge;
    }

    /**
     * Setter.
     */
    public void setHalfEdge(HalfEdge halfEdge) {
        this.halfEdge = halfEdge;
    }

    /**
     * Getter.
     */
    public Point3f getPosition() {
        return position;
    }

    /**
     * Setter
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Getter
     */
    public String getName() {
        return name;
    }
}

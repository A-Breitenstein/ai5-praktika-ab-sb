/**
 * Prof. Philipp Jenke
 * Hochschule für Angewandte Wissenschaften (HAW), Hamburg
 * Lecture demo program.
 */
package aufgabe4;

import javax.vecmath.Point3f;

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

    /**
     * Reference to an adjacent half edge
     */
    private HalfEdge halfEdge = null;

    /**
     * Constructor
     * 
     * @param position
     *            Position of the vertex.
     */
    public HalfEdgeVertex(Point3f position) {
        this.position = position;
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

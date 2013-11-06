/**
 * Prof. Philipp Jenke
 * Hochschule für Angewandte Wissenschaften (HAW), Hamburg
 * Lecture demo program.
 */
package aufgabe4;

/**
 * Representation of a half edge in the half edge data structure
 * 
 * @author Philipp Jenke
 * 
 */
public class HalfEdge {

    /**
     * Opposite half edge
     */
    private HalfEdge opposite = null;

    /**
     * Reference to the (start) vertex
     */
    private HalfEdgeVertex vertex = null;

    /**
     * Reference to the previous edge in the data structure.
     */
    private HalfEdge prev = null;

    /**
     * Reference to the next edge in the data structure.
     */
    private HalfEdge next = null;

    /**
     * Reference to the facet
     */
    private IHalfEdgeFacet facet = null;

    /**
     * Name, used for debugging purposes.
     */
    private String name;

    /**
     * Constructor
     */
    public HalfEdge() {

    }

    /**
     * Getter.
     */
    public HalfEdge getOpposite() {
        return opposite;
    }

    /**
     * Setter.
     */
    public void setOpposite(HalfEdge oppsite) {
        this.opposite = oppsite;
    }

    /**
     * Getter.
     */
    public HalfEdgeVertex getVertex() {
        return vertex;
    }

    /**
     * Setter.
     */
    public void setVertex(HalfEdgeVertex vertex) {
        this.vertex = vertex;
    }

    /**
     * Getter.
     */
    public HalfEdge getPrev() {
        return prev;
    }

    /**
     * Setter.
     */
    public void setPrev(HalfEdge prev) {
        this.prev = prev;
    }

    /**
     * Getter.
     */
    public HalfEdge getNext() {
        return next;
    }

    /**
     * Setter.
     */
    public void setNext(HalfEdge next) {
        this.next = next;
    }

    /**
     * Getter.
     */
    public IHalfEdgeFacet getFacet() {
        return facet;
    }

    /**
     * Setter.
     */
    public void setFacet(IHalfEdgeFacet facet) {
        this.facet = facet;
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

/**
 * Prof. Philipp Jenke
 * Hochschule für Angewandte Wissenschaften (HAW), Hamburg
 * Lecture demo program.
 */
package aufgabe4;

/**
 * Interface for all facet types in the half edge data structure.
 * 
 * @author Philipp Jenke
 * 
 */
public abstract class IHalfEdgeFacet {
    /**
     * Reference to an incident half edge.
     */
    protected HalfEdge halfEdge = null;

    /**
     * Constructor.
     */
    public IHalfEdgeFacet() {
    }

    /**
     * Getter.
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

}

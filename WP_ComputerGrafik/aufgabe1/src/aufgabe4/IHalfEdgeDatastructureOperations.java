/**
 * Prof. Philipp Jenke
 * Hochschule für Angewandte Wissenschaften (HAW), Hamburg
 * Lecture demo program.
 */
package aufgabe4;

import java.util.List;

/**
 * Interface for a class containing operations based on the half edge data
 * structure.
 * 
 * @author Philipp Jenke
 * 
 */
public interface IHalfEdgeDatastructureOperations extends IHalfEdgeDatastructure{

    /**
     * Return a list of adjacent vertices for a given vertex.
     * 
     * @param vertex
     *            Initial vertex.
     * @return List of adjacent vertices.
     */
    public List<HalfEdgeVertex> getAdjacentVertices(HalfEdgeVertex vertex);

    /**
     * Return a list of incident facets of a vertex.
     * 
     * @param vertex
     *            Central query vertex.
     * @return List of incident facets.
     */
    public List<IHalfEdgeFacet> getIncidentFacets(HalfEdgeVertex vertex);

    /**
     * Return a list if adjacent half edges (incoming and outgoing) for a given
     * vertex.
     * 
     * @param vertex
     *            Central query vertex.
     * @return List of incident half edges.
     */
    public List<HalfEdge> getIncidetEdges(HalfEdgeVertex vertex);

    /**
     * Return a list of incident vertices to a given facet.
     * 
     * @param facet
     *            Query facet.
     * @return List of incident vertices.
     */
    public List<HalfEdgeVertex> getIncidentVertices(IHalfEdgeFacet facet);

    /**
     * Return the list of incident facets of a given facet.
     * 
     * @param facet
     *            Query facet.
     * @return List of incident facets.
     */
    public List<IHalfEdgeFacet> getIncidentFacets(IHalfEdgeFacet facet);

    /**
     * Return a list of incident half edges for a given facet. Only the half
     * edges corresponding to the facet are returned, opposite edges are ignored.
     * 
     * @param facet
     *            Query facet.
     * @return List of incident facets.
     */
    public List<HalfEdge> getIncidentHalfEdges(IHalfEdgeFacet facet);
}

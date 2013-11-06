/**
 * Prof. Philipp Jenke
 * Hochschule für Angewandte Wissenschaften (HAW), Hamburg
 * Lecture demo program.
 */
package aufgabe4;

import java.util.ArrayList;
import java.util.List;

/**
 * Container for a half edge data structure.
 * 
 * @author Philipp Jenke
 * 
 */
public class HalfEdgeDatastructure implements IHalfEdgeDatastructure {

    /**
     * List of vertices.
     */
    private List<HalfEdgeVertex> vertices = new ArrayList<HalfEdgeVertex>();

    /**
     * List of facets.
     */
    private List<IHalfEdgeFacet> facets = new ArrayList<IHalfEdgeFacet>();

    /**
     * List of half edges.
     */
    private List<HalfEdge> halfEdges = new ArrayList<HalfEdge>();

    /**
     * Constructor.
     */
    public HalfEdgeDatastructure() {

    }

    /*
     * (nicht-Javadoc)
     * 
     * @see edu.cg1.exercises.neighbordatastructure.IHalfEdgeDataStructure#
     * getNumberOfVertices()
     */
    @Override
    public int getNumberOfVertices() {
        return vertices.size();
    }

    /*
     * (nicht-Javadoc)
     * 
     * @see edu.cg1.exercises.neighbordatastructure.IHalfEdgeDataStructure#
     * getNumberOfFacets()
     */
    @Override
    public int getNumberOfFacets() {
        return facets.size();
    }

    /*
     * (nicht-Javadoc)
     * 
     * @see edu.cg1.exercises.neighbordatastructure.IHalfEdgeDataStructure#
     * getNumberOfHalfEdges()
     */
    @Override
    public int getNumberOfHalfEdges() {
        return halfEdges.size();
    }

    /*
     * (nicht-Javadoc)
     * 
     * @see
     * edu.cg1.exercises.neighbordatastructure.IHalfEdgeDataStructure#getVertex
     * ()
     */
    @Override
    public HalfEdgeVertex getVertex(int index) {
        return vertices.get(index);
    }

    /*
     * (nicht-Javadoc)
     * 
     * @see
     * edu.cg1.exercises.neighbordatastructure.IHalfEdgeDataStructure#getFacet
     * (int)
     */
    @Override
    public IHalfEdgeFacet getFacet(int index) {
        return facets.get(index);
    }

    /*
     * (nicht-Javadoc)
     * 
     * @see
     * edu.cg1.exercises.neighbordatastructure.IHalfEdgeDataStructure#getHalfEdge
     * (int)
     */
    @Override
    public HalfEdge getHalfEdge(int index) {
        return halfEdges.get(index);
    }

    /*
     * (nicht-Javadoc)
     * 
     * @see
     * edu.cg1.exercises.neighbordatastructure.IHalfEdgeDatastructure#addVertex
     * (edu.cg1.exercises.neighbordatastructure.Vertex)
     */
    @Override
    public int addVertex(HalfEdgeVertex vertex) {
        vertices.add(vertex);
        vertex.setName("" + (vertices.size() - 1));
        return vertices.size() - 1;
    }

    /*
     * (nicht-Javadoc)
     * 
     * @see
     * edu.cg1.exercises.neighbordatastructure.IHalfEdgeDatastructure#addFacet
     * (edu.cg1.exercises.neighbordatastructure.Facet)
     */
    @Override
    public int addFacet(IHalfEdgeFacet facet) {
        facets.add(facet);
        return facets.size() - 1;
    }

    /*
     * (nicht-Javadoc)
     * 
     * @see
     * edu.cg1.exercises.neighbordatastructure.IHalfEdgeDatastructure#addHalfEdge
     * (edu.cg1.exercises.neighbordatastructure.HalfEdge)
     */
    @Override
    public int addHalfEdge(HalfEdge halfEdge) {
        halfEdges.add(halfEdge);
        halfEdge.setName("" + (halfEdges.size() - 1));
        return halfEdges.size() - 1;
    }

}

/**
 * Prof. Philipp Jenke
 * Hochschule für Angewandte Wissenschaften (HAW), Hamburg
 * Lecture demo program.
 */
package aufgabe4.tests;

import aufgabe4.HalfEdge;
import aufgabe4.HalfEdgeVertex;
import aufgabe4.IHalfEdgeDatastructure;
import aufgabe4.IHalfEdgeFacet;
import org.junit.Test;

import java.util.List;
import static org.junit.Assert.*;
/**
 * Test class for the half edge data structure operations.
 * 
 * @author Philipp Jenke
 * 
 */
public class TestHalfEdgeDatastructureOperations {

    /**
     * Test query vertex -> vertex
     */
    @Test
    public void testQueryVertexVertex() {
        IHalfEdgeDatastructure ds = TestHalfEdgeDatastructure
                .createHalfEdgeDatastructure();
        HalfEdgeDatastructureOperations ops = new HalfEdgeDatastructureOperations();
        List<HalfEdgeVertex> queryResult = ops.getAdjacentVertices(ds
                .getVertex(2));
        HalfEdgeVertex[] expected = { ds.getVertex(0), ds.getVertex(1),
                ds.getVertex(6), ds.getVertex(3) };
        assertEquals("Query vertex -> vertex failed.", expected.length,
                queryResult.size());
        for (HalfEdgeVertex vertex : expected) {
            assertTrue("Query vertex -> vertex failed.",
                    queryResult.contains(vertex));
        }
    }

    /**
     * Test query vertex -> facet
     */
    @Test
    public void testQueryVertexFacet() {
        IHalfEdgeDatastructure ds = TestHalfEdgeDatastructure
                .createHalfEdgeDatastructure();
        HalfEdgeDatastructureOperations ops = new HalfEdgeDatastructureOperations();
        List<IHalfEdgeFacet> queryResult = ops.getIncidentFacets(ds
                .getVertex(2));
        IHalfEdgeFacet[] expected = { ds.getFacet(0), ds.getFacet(1),
                ds.getFacet(2), ds.getFacet(4) };
        assertEquals("Query vertex -> facet failed.", expected.length,
                queryResult.size());
        for (IHalfEdgeFacet facet : expected) {
            assertTrue("Query vertex -> facet failed.",
                    queryResult.contains(facet));
        }
    }

    /**
     * Test query vertex -> half edge
     */
    @Test
    public void testQueryVertexHalfEdge() {
        IHalfEdgeDatastructure ds = TestHalfEdgeDatastructure
                .createHalfEdgeDatastructure();
        HalfEdgeDatastructureOperations ops = new HalfEdgeDatastructureOperations();
        List<HalfEdge> queryResult = ops.getIncidetEdges(ds.getVertex(2));
        HalfEdge[] expected = { ds.getHalfEdge(0), ds.getHalfEdge(5),
                ds.getHalfEdge(4), ds.getHalfEdge(6), ds.getHalfEdge(8),
                ds.getHalfEdge(12), ds.getHalfEdge(1), ds.getHalfEdge(14) };
        assertEquals("Query vertex -> half edge failed.", expected.length,
                queryResult.size());
        for (HalfEdge halfEdge : expected) {
            assertTrue("Query vertex -> half edge failed.",
                    queryResult.contains(halfEdge));
        }
    }

    /**
     * Test query facet -> vertex
     */
    @Test
    public void testQueryFacetVertex() {
        IHalfEdgeDatastructure ds = TestHalfEdgeDatastructure
                .createHalfEdgeDatastructure();
        HalfEdgeDatastructureOperations ops = new HalfEdgeDatastructureOperations();
        List<HalfEdgeVertex> queryResult = ops.getIncidentVertices(ds
                .getFacet(4));
        HalfEdgeVertex[] expected = { ds.getVertex(2), ds.getVertex(3),
                ds.getVertex(6) };
        assertEquals("Query facet -> vertex failed.", expected.length,
                queryResult.size());
        for (HalfEdgeVertex vertex : expected) {
            assertTrue("Query facet -> vertex failed.",
                    queryResult.contains(vertex));
        }
    }

    /**
     * Test query facet -> facet
     */
    @Test
    public void testQueryFacetFacet() {
        IHalfEdgeDatastructure ds = TestHalfEdgeDatastructure
                .createHalfEdgeDatastructure();
        HalfEdgeDatastructureOperations ops = new HalfEdgeDatastructureOperations();
        List<IHalfEdgeFacet> queryResult = ops
                .getIncidentFacets(ds.getFacet(4));
        IHalfEdgeFacet[] expected = { ds.getFacet(0), ds.getFacet(2),
                ds.getFacet(5) };
        assertEquals("Query facet -> facet failed.", expected.length,
                queryResult.size());
        for (IHalfEdgeFacet facet : expected) {
            assertTrue("Query facet -> facet failed.",
                    queryResult.contains(facet));
        }
    }

    /**
     * Test query facet -> half edge
     */
    @Test
    public void testQueryFacetHalfEdge() {
        IHalfEdgeDatastructure ds = TestHalfEdgeDatastructure
                .createHalfEdgeDatastructure();
        HalfEdgeDatastructureOperations ops = new HalfEdgeDatastructureOperations();
        List<HalfEdge> queryResult = ops.getIncidentHalfEdges(ds.getFacet(4));
        HalfEdge[] expected = { ds.getHalfEdge(12), ds.getHalfEdge(13),
                ds.getHalfEdge(14) };
        assertEquals("Query facet -> half edge failed.", expected.length,
                queryResult.size());
        for (HalfEdge halfEdge : expected) {
            assertTrue("Query facet -> half edge failed.",
                    queryResult.contains(halfEdge));
        }
    }
}

/**
 * Prof. Philipp Jenke
 * Hochschule für Angewandte Wissenschaften (HAW), Hamburg
 * Lecture demo program.
 */
package aufgabe4.tests;

import aufgabe4.*;
import org.junit.Test;

import javax.vecmath.Point3f;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;

/**
 * Testing functionality for the half edge data structure
 * 
 * @author Philipp Jenke
 * 
 */
public class TestHalfEdgeDatastructureTetraeder {


    /**
     * Test the link information in all vertices
     */
    @Test
    public void testVertices() {
        IHalfEdgeDatastructure ds = createHalfEdgeDatastructure();
        for (int i = 0; i < ds.getNumberOfVertices(); i++) {
            assertNotSame("Invalid vertex link.", ds.getVertex(i)
                    .getHalfEdge(), null);
        }
    }

    /**
     * Test the link information in all facets
     */
    @Test
    public void testFacets() {
        IHalfEdgeDatastructure ds = createHalfEdgeDatastructure();
        for (int i = 0; i < ds.getNumberOfFacets(); i++) {
            assertNotSame("Invalid facet link.",
                    ds.getFacet(i).getHalfEdge(), null);
            assertEquals(ds.getFacet(i).getHalfEdge().getFacet(),
                    ds.getFacet(i));
        }
    }

    /**
     * Test the link information in all half edges
     */
    @Test
    public void testHalfEdges() {
        IHalfEdgeDatastructure ds = createHalfEdgeDatastructure();
        for (int i = 0; i < ds.getNumberOfHalfEdges(); i++) {

            // Test next() -> prev()
            assertNotSame("Invalid half edge link.", ds.getHalfEdge(i)
                    .getNext(), null);
            assertEquals("Invalid half edge link.", ds.getHalfEdge(i).getNext()
                    .getPrev(), ds.getHalfEdge(i));

            // Test prev() -> next()
            assertNotSame("Invalid half edge link.", ds.getHalfEdge(i)
                    .getPrev(), null);
            assertEquals("Invalid half edge link.", ds.getHalfEdge(i).getPrev()
                    .getNext(), ds.getHalfEdge(i));
            // Test facet link
            assertEquals("Invalid half edge link.", ds.getHalfEdge(i)
                    .getFacet(), ds.getHalfEdge(i).getNext().getFacet());
        }
    }

    /**
     * Helper method for all query tests - create a half edge data structure for
     * a cube.
     * 
     * @return Half edge data structure.
     */
    public static IHalfEdgeDatastructure createHalfEdgeDatastructure() {
        return Tetraeder.create().convertToHalfEdgeDatastructure();
    }

}

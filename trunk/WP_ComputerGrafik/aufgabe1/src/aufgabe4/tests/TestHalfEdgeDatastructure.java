/**
 * Prof. Philipp Jenke
 * Hochschule für Angewandte Wissenschaften (HAW), Hamburg
 * Lecture demo program.
 */
package aufgabe4.tests;

import static org.junit.Assert.*;

import javax.vecmath.Point3f;

import aufgabe4.*;
import org.junit.Test;

/**
 * Testing functionality for the half edge data structure
 * 
 * @author Philipp Jenke
 * 
 */
public class TestHalfEdgeDatastructure {


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
        IHalfEdgeDatastructure ds = new HalfEdgeDatastructure();

        // Create and insert vertices
        ds.addVertex(new HalfEdgeVertex(new Point3f(0, 0, 0)));
        ds.addVertex(new HalfEdgeVertex(new Point3f(1, 0, 0)));
        ds.addVertex(new HalfEdgeVertex(new Point3f(1, 1, 0)));
        ds.addVertex(new HalfEdgeVertex(new Point3f(0, 1, 0)));
        ds.addVertex(new HalfEdgeVertex(new Point3f(0, 0, 1)));
        ds.addVertex(new HalfEdgeVertex(new Point3f(1, 0, 1)));
        ds.addVertex(new HalfEdgeVertex(new Point3f(1, 1, 1)));
        ds.addVertex(new HalfEdgeVertex(new Point3f(0, 1, 1)));

        // Create and insert facets
        for (int i = 0; i < 12; i++) {
            ds.addFacet(new HalfEdgeTriangle());
        }

        // Create and insert half edges
        for (int i = 0; i < 36; i++) {
            ds.addHalfEdge(new HalfEdge());
        }

        // Link vertices
        ds.getVertex(0).setHalfEdge(ds.getHalfEdge(0));
        ds.getVertex(1).setHalfEdge(ds.getHalfEdge(9));
        ds.getVertex(2).setHalfEdge(ds.getHalfEdge(6));
        ds.getVertex(3).setHalfEdge(ds.getHalfEdge(2));
        ds.getVertex(4).setHalfEdge(ds.getHalfEdge(31));
        ds.getVertex(5).setHalfEdge(ds.getHalfEdge(10));
        ds.getVertex(6).setHalfEdge(ds.getHalfEdge(16));
        ds.getVertex(7).setHalfEdge(ds.getHalfEdge(17));

        // Link facets
        ds.getFacet(0).setHalfEdge(ds.getHalfEdge(0));
        ds.getFacet(1).setHalfEdge(ds.getHalfEdge(3));
        ds.getFacet(2).setHalfEdge(ds.getHalfEdge(6));
        ds.getFacet(3).setHalfEdge(ds.getHalfEdge(9));
        ds.getFacet(4).setHalfEdge(ds.getHalfEdge(12));
        ds.getFacet(5).setHalfEdge(ds.getHalfEdge(15));
        ds.getFacet(6).setHalfEdge(ds.getHalfEdge(18));
        ds.getFacet(7).setHalfEdge(ds.getHalfEdge(21));
        ds.getFacet(8).setHalfEdge(ds.getHalfEdge(24));
        ds.getFacet(9).setHalfEdge(ds.getHalfEdge(27));
        ds.getFacet(10).setHalfEdge(ds.getHalfEdge(33));
        ds.getFacet(11).setHalfEdge(ds.getHalfEdge(30));

        // Link half edges
        ds.getHalfEdge(0).setNext(ds.getHalfEdge(1));
        ds.getHalfEdge(0).setPrev(ds.getHalfEdge(2));
        ds.getHalfEdge(0).setOpposite(ds.getHalfEdge(5));
        ds.getHalfEdge(0).setVertex(ds.getVertex(0));
        ds.getHalfEdge(0).setFacet(ds.getFacet(0));
        ds.getHalfEdge(1).setNext(ds.getHalfEdge(2));
        ds.getHalfEdge(1).setPrev(ds.getHalfEdge(0));
        ds.getHalfEdge(1).setOpposite(ds.getHalfEdge(14));
        ds.getHalfEdge(1).setVertex(ds.getVertex(2));
        ds.getHalfEdge(1).setFacet(ds.getFacet(0));
        ds.getHalfEdge(2).setNext(ds.getHalfEdge(0));
        ds.getHalfEdge(2).setPrev(ds.getHalfEdge(1));
        ds.getHalfEdge(2).setOpposite(ds.getHalfEdge(27));
        ds.getHalfEdge(2).setVertex(ds.getVertex(3));
        ds.getHalfEdge(2).setFacet(ds.getFacet(0));
        ds.getHalfEdge(3).setNext(ds.getHalfEdge(4));
        ds.getHalfEdge(3).setPrev(ds.getHalfEdge(5));
        ds.getHalfEdge(3).setOpposite(ds.getHalfEdge(32));
        ds.getHalfEdge(3).setVertex(ds.getVertex(0));
        ds.getHalfEdge(3).setFacet(ds.getFacet(1));
        ds.getHalfEdge(4).setNext(ds.getHalfEdge(5));
        ds.getHalfEdge(4).setPrev(ds.getHalfEdge(3));
        ds.getHalfEdge(4).setOpposite(ds.getHalfEdge(6));
        ds.getHalfEdge(4).setVertex(ds.getVertex(1));
        ds.getHalfEdge(4).setFacet(ds.getFacet(1));
        ds.getHalfEdge(5).setNext(ds.getHalfEdge(3));
        ds.getHalfEdge(5).setPrev(ds.getHalfEdge(4));
        ds.getHalfEdge(5).setOpposite(ds.getHalfEdge(0));
        ds.getHalfEdge(5).setVertex(ds.getVertex(2));
        ds.getHalfEdge(5).setFacet(ds.getFacet(1));
        ds.getHalfEdge(6).setNext(ds.getHalfEdge(7));
        ds.getHalfEdge(6).setPrev(ds.getHalfEdge(8));
        ds.getHalfEdge(6).setOpposite(ds.getHalfEdge(4));
        ds.getHalfEdge(6).setVertex(ds.getVertex(2));
        ds.getHalfEdge(6).setFacet(ds.getFacet(2));
        ds.getHalfEdge(7).setNext(ds.getHalfEdge(8));
        ds.getHalfEdge(7).setPrev(ds.getHalfEdge(6));
        ds.getHalfEdge(7).setOpposite(ds.getHalfEdge(11));
        ds.getHalfEdge(7).setVertex(ds.getVertex(1));
        ds.getHalfEdge(7).setFacet(ds.getFacet(2));
        ds.getHalfEdge(8).setNext(ds.getHalfEdge(6));
        ds.getHalfEdge(8).setPrev(ds.getHalfEdge(7));
        ds.getHalfEdge(8).setOpposite(ds.getHalfEdge(12));
        ds.getHalfEdge(8).setVertex(ds.getVertex(6));
        ds.getHalfEdge(8).setFacet(ds.getFacet(2));
        ds.getHalfEdge(9).setNext(ds.getHalfEdge(10));
        ds.getHalfEdge(9).setPrev(ds.getHalfEdge(11));
        ds.getHalfEdge(9).setOpposite(ds.getHalfEdge(35));
        ds.getHalfEdge(9).setVertex(ds.getVertex(1));
        ds.getHalfEdge(9).setFacet(ds.getFacet(3));
        ds.getHalfEdge(10).setNext(ds.getHalfEdge(11));
        ds.getHalfEdge(10).setPrev(ds.getHalfEdge(9));
        ds.getHalfEdge(10).setOpposite(ds.getHalfEdge(20));
        ds.getHalfEdge(10).setVertex(ds.getVertex(5));
        ds.getHalfEdge(10).setFacet(ds.getFacet(3));
        ds.getHalfEdge(11).setNext(ds.getHalfEdge(9));
        ds.getHalfEdge(11).setPrev(ds.getHalfEdge(10));
        ds.getHalfEdge(11).setOpposite(ds.getHalfEdge(7));
        ds.getHalfEdge(11).setVertex(ds.getVertex(6));
        ds.getHalfEdge(11).setFacet(ds.getFacet(3));
        ds.getHalfEdge(12).setNext(ds.getHalfEdge(13));
        ds.getHalfEdge(12).setPrev(ds.getHalfEdge(14));
        ds.getHalfEdge(12).setOpposite(ds.getHalfEdge(8));
        ds.getHalfEdge(12).setVertex(ds.getVertex(2));
        ds.getHalfEdge(12).setFacet(ds.getFacet(4));
        ds.getHalfEdge(13).setNext(ds.getHalfEdge(14));
        ds.getHalfEdge(13).setPrev(ds.getHalfEdge(12));
        ds.getHalfEdge(13).setOpposite(ds.getHalfEdge(15));
        ds.getHalfEdge(13).setVertex(ds.getVertex(6));
        ds.getHalfEdge(13).setFacet(ds.getFacet(4));
        ds.getHalfEdge(14).setNext(ds.getHalfEdge(12));
        ds.getHalfEdge(14).setPrev(ds.getHalfEdge(13));
        ds.getHalfEdge(14).setOpposite(ds.getHalfEdge(1));
        ds.getHalfEdge(14).setVertex(ds.getVertex(3));
        ds.getHalfEdge(14).setFacet(ds.getFacet(4));
        ds.getHalfEdge(15).setNext(ds.getHalfEdge(16));
        ds.getHalfEdge(15).setPrev(ds.getHalfEdge(17));
        ds.getHalfEdge(15).setOpposite(ds.getHalfEdge(13));
        ds.getHalfEdge(15).setVertex(ds.getVertex(3));
        ds.getHalfEdge(15).setFacet(ds.getFacet(5));
        ds.getHalfEdge(16).setNext(ds.getHalfEdge(17));
        ds.getHalfEdge(16).setPrev(ds.getHalfEdge(15));
        ds.getHalfEdge(16).setOpposite(ds.getHalfEdge(19));
        ds.getHalfEdge(16).setVertex(ds.getVertex(6));
        ds.getHalfEdge(16).setFacet(ds.getFacet(5));
        ds.getHalfEdge(17).setNext(ds.getHalfEdge(15));
        ds.getHalfEdge(17).setPrev(ds.getHalfEdge(16));
        ds.getHalfEdge(17).setOpposite(ds.getHalfEdge(24));
        ds.getHalfEdge(17).setVertex(ds.getVertex(7));
        ds.getHalfEdge(17).setFacet(ds.getFacet(5));
        ds.getHalfEdge(18).setNext(ds.getHalfEdge(19));
        ds.getHalfEdge(18).setPrev(ds.getHalfEdge(20));
        ds.getHalfEdge(18).setOpposite(ds.getHalfEdge(21));
        ds.getHalfEdge(18).setVertex(ds.getVertex(5));
        ds.getHalfEdge(18).setFacet(ds.getFacet(6));
        ds.getHalfEdge(19).setNext(ds.getHalfEdge(20));
        ds.getHalfEdge(19).setPrev(ds.getHalfEdge(18));
        ds.getHalfEdge(19).setOpposite(ds.getHalfEdge(16));
        ds.getHalfEdge(19).setVertex(ds.getVertex(7));
        ds.getHalfEdge(19).setFacet(ds.getFacet(6));
        ds.getHalfEdge(20).setNext(ds.getHalfEdge(18));
        ds.getHalfEdge(20).setPrev(ds.getHalfEdge(19));
        ds.getHalfEdge(20).setOpposite(ds.getHalfEdge(10));
        ds.getHalfEdge(20).setVertex(ds.getVertex(6));
        ds.getHalfEdge(20).setFacet(ds.getFacet(6));
        ds.getHalfEdge(21).setNext(ds.getHalfEdge(22));
        ds.getHalfEdge(21).setPrev(ds.getHalfEdge(23));
        ds.getHalfEdge(21).setOpposite(ds.getHalfEdge(18));
        ds.getHalfEdge(21).setVertex(ds.getVertex(7));
        ds.getHalfEdge(21).setFacet(ds.getFacet(7));
        ds.getHalfEdge(22).setNext(ds.getHalfEdge(23));
        ds.getHalfEdge(22).setPrev(ds.getHalfEdge(21));
        ds.getHalfEdge(22).setOpposite(ds.getHalfEdge(34));
        ds.getHalfEdge(22).setVertex(ds.getVertex(5));
        ds.getHalfEdge(22).setFacet(ds.getFacet(7));
        ds.getHalfEdge(23).setNext(ds.getHalfEdge(21));
        ds.getHalfEdge(23).setPrev(ds.getHalfEdge(22));
        ds.getHalfEdge(23).setOpposite(ds.getHalfEdge(25));
        ds.getHalfEdge(23).setVertex(ds.getVertex(4));
        ds.getHalfEdge(23).setFacet(ds.getFacet(7));
        ds.getHalfEdge(24).setNext(ds.getHalfEdge(25));
        ds.getHalfEdge(24).setPrev(ds.getHalfEdge(26));
        ds.getHalfEdge(24).setOpposite(ds.getHalfEdge(17));
        ds.getHalfEdge(24).setVertex(ds.getVertex(3));
        ds.getHalfEdge(24).setFacet(ds.getFacet(8));
        ds.getHalfEdge(25).setNext(ds.getHalfEdge(26));
        ds.getHalfEdge(25).setPrev(ds.getHalfEdge(24));
        ds.getHalfEdge(25).setOpposite(ds.getHalfEdge(23));
        ds.getHalfEdge(25).setVertex(ds.getVertex(7));
        ds.getHalfEdge(25).setFacet(ds.getFacet(8));
        ds.getHalfEdge(26).setNext(ds.getHalfEdge(24));
        ds.getHalfEdge(26).setPrev(ds.getHalfEdge(25));
        ds.getHalfEdge(26).setOpposite(ds.getHalfEdge(28));
        ds.getHalfEdge(26).setVertex(ds.getVertex(4));
        ds.getHalfEdge(26).setFacet(ds.getFacet(8));
        ds.getHalfEdge(27).setNext(ds.getHalfEdge(28));
        ds.getHalfEdge(27).setPrev(ds.getHalfEdge(29));
        ds.getHalfEdge(27).setOpposite(ds.getHalfEdge(2));
        ds.getHalfEdge(27).setVertex(ds.getVertex(0));
        ds.getHalfEdge(27).setFacet(ds.getFacet(9));
        ds.getHalfEdge(28).setNext(ds.getHalfEdge(29));
        ds.getHalfEdge(28).setPrev(ds.getHalfEdge(27));
        ds.getHalfEdge(28).setOpposite(ds.getHalfEdge(26));
        ds.getHalfEdge(28).setVertex(ds.getVertex(3));
        ds.getHalfEdge(28).setFacet(ds.getFacet(9));
        ds.getHalfEdge(29).setNext(ds.getHalfEdge(27));
        ds.getHalfEdge(29).setPrev(ds.getHalfEdge(28));
        ds.getHalfEdge(29).setOpposite(ds.getHalfEdge(30));
        ds.getHalfEdge(29).setVertex(ds.getVertex(4));
        ds.getHalfEdge(29).setFacet(ds.getFacet(9));
        ds.getHalfEdge(30).setNext(ds.getHalfEdge(31));
        ds.getHalfEdge(30).setPrev(ds.getHalfEdge(32));
        ds.getHalfEdge(30).setOpposite(ds.getHalfEdge(29));
        ds.getHalfEdge(30).setVertex(ds.getVertex(0));
        ds.getHalfEdge(30).setFacet(ds.getFacet(11));
        ds.getHalfEdge(31).setNext(ds.getHalfEdge(32));
        ds.getHalfEdge(31).setPrev(ds.getHalfEdge(30));
        ds.getHalfEdge(31).setOpposite(ds.getHalfEdge(33));
        ds.getHalfEdge(31).setVertex(ds.getVertex(4));
        ds.getHalfEdge(31).setFacet(ds.getFacet(11));
        ds.getHalfEdge(32).setNext(ds.getHalfEdge(30));
        ds.getHalfEdge(32).setPrev(ds.getHalfEdge(31));
        ds.getHalfEdge(32).setOpposite(ds.getHalfEdge(3));
        ds.getHalfEdge(32).setVertex(ds.getVertex(1));
        ds.getHalfEdge(32).setFacet(ds.getFacet(11));
        ds.getHalfEdge(33).setNext(ds.getHalfEdge(34));
        ds.getHalfEdge(33).setPrev(ds.getHalfEdge(35));
        ds.getHalfEdge(33).setOpposite(ds.getHalfEdge(31));
        ds.getHalfEdge(33).setVertex(ds.getVertex(1));
        ds.getHalfEdge(33).setFacet(ds.getFacet(10));
        ds.getHalfEdge(34).setNext(ds.getHalfEdge(35));
        ds.getHalfEdge(34).setPrev(ds.getHalfEdge(33));
        ds.getHalfEdge(34).setOpposite(ds.getHalfEdge(22));
        ds.getHalfEdge(34).setVertex(ds.getVertex(4));
        ds.getHalfEdge(34).setFacet(ds.getFacet(10));
        ds.getHalfEdge(35).setNext(ds.getHalfEdge(33));
        ds.getHalfEdge(35).setPrev(ds.getHalfEdge(34));
        ds.getHalfEdge(35).setOpposite(ds.getHalfEdge(9));
        ds.getHalfEdge(35).setVertex(ds.getVertex(5));
        ds.getHalfEdge(35).setFacet(ds.getFacet(10));

        return ds;
    }

}

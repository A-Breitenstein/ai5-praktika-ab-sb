package aufgabe4;

import utils.CGkursUtils;

import java.util.ArrayList;
import java.util.List;

import static utils.CGkursUtils.isEqual;

/**
 * Created with IntelliJ IDEA.
 * User: abg667
 * Date: 12.11.13
 * Time: 12:40
 * To change this template use File | Settings | File Templates.
 */
public class HalfEdgeDatastructureOperations implements IHalfEdgeDatastructureOperations {
    IHalfEdgeDatastructure hed;

    private HalfEdgeDatastructureOperations(IHalfEdgeDatastructure hed) {
            this.hed = hed;

    }

    public static HalfEdgeDatastructureOperations create(IHalfEdgeDatastructure hed) {
        return new HalfEdgeDatastructureOperations(hed);
    }

    @Override
    public List<HalfEdgeVertex> getAdjacentVertices(HalfEdgeVertex vertex) {
        List<HalfEdgeVertex> result = new ArrayList<HalfEdgeVertex>();

        for (HalfEdge edge : getIncidetEdges(vertex)) {
            if ( ! isEqual(edge.getVertex().getPosition(), vertex.getPosition())) {
                result.add(edge.getVertex());
            }
        }
        return result;
    }

    @Override
    public List<IHalfEdgeFacet> getIncidentFacets(HalfEdgeVertex vertex) {
        List<IHalfEdgeFacet> result = new ArrayList<IHalfEdgeFacet>();
        for (HalfEdge edge : getIncidentOutgoingEdges(vertex)) {
            result.add(edge.getFacet());
        }
        return result;
    }

    @Override
    public List<HalfEdge> getIncidetEdges(HalfEdgeVertex vertex) {
        HalfEdge edge;
        List<HalfEdge> result = new ArrayList<HalfEdge>();

        HalfEdge startEdge = vertex.getHalfEdge();
        result.add(startEdge);

        HalfEdge tempEdge = startEdge.getOpposite();

        boolean imKreisgelaufen = false;

        while (!imKreisgelaufen) {
            result.add(tempEdge);
            tempEdge = tempEdge.getNext();


            if (startEdge.equals(tempEdge)) {
                imKreisgelaufen = true;
            } else {
                result.add(tempEdge);
            }

            tempEdge = tempEdge.getOpposite();
        }
        return result;
    }



    private List<HalfEdge> getIncidentOutgoingEdges(HalfEdgeVertex vertex) {
        List<HalfEdge> result = new ArrayList<HalfEdge>();
        for (HalfEdge edge : getIncidetEdges(vertex)) {
            if(edge.getVertex().equals(vertex))
                result.add(edge);
        }
        return result;
    }

    @Override
    public List<HalfEdgeVertex> getIncidentVertices(IHalfEdgeFacet facet) {
        List<HalfEdgeVertex> result = new ArrayList<HalfEdgeVertex>();
        for (HalfEdge edge : getIncidentHalfEdges(facet)) {
            result.add(edge.getVertex());
        }
        return result;
    }

    @Override
    public List<IHalfEdgeFacet> getIncidentFacets(IHalfEdgeFacet facet) {
        List<IHalfEdgeFacet> result = new ArrayList<IHalfEdgeFacet>();
        for (HalfEdge edge : getIncidentHalfEdges(facet)) {
            result.add(edge.getOpposite().getFacet());
        }
        return result;
    }

    @Override
    public List<HalfEdge> getIncidentHalfEdges(IHalfEdgeFacet facet) {
        List<HalfEdge> result = new ArrayList<HalfEdge>();
        HalfEdge edge = facet.getHalfEdge();
        HalfEdge nextEdge = edge.getNext();

        result.add(edge);
        result.add(nextEdge);
        result.add(nextEdge.getNext());

        return result;
    }

    @Override
    public int getNumberOfVertices() {
        return hed.getNumberOfVertices();
    }

    @Override
    public int getNumberOfFacets() {
        return hed.getNumberOfFacets();
    }

    @Override
    public int getNumberOfHalfEdges() {
        return hed.getNumberOfHalfEdges();
    }

    @Override
    public HalfEdgeVertex getVertex(int index) {
        return hed.getVertex(index);
    }

    @Override
    public IHalfEdgeFacet getFacet(int index) {
        return hed.getFacet(index);
    }

    @Override
    public HalfEdge getHalfEdge(int index) {
        return hed.getHalfEdge(index);
    }

    @Override
    public int addVertex(HalfEdgeVertex vertex) {
        return hed.addVertex(vertex);
    }

    @Override
    public int addFacet(IHalfEdgeFacet facet) {
        return hed.addFacet(facet);
    }

    @Override
    public int addHalfEdge(HalfEdge halfEdge) {
        return hed.addHalfEdge(halfEdge);
    }
}

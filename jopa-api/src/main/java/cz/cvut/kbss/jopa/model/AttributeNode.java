package cz.cvut.kbss.jopa.model;

import java.util.Map;

/**
 * Represents an attribute node of an entity graph.
 *
 * @param <T> The type of the attribute
 */
public interface AttributeNode<T> {

    /**
     * Gets the name of the attribute corresponding to the attribute node.
     *
     * @return name of the attribute
     */
    String getAttributeName();

    /**
     * Return the map of subgraphs associated with this attribute node.
     *
     * @return Map of subgraphs associated with this attribute node or empty Map if none have been defined
     */
    Map<Class, Subgraph> getSubgraphs();
}

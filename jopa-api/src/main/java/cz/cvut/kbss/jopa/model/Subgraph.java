package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;

import java.util.List;

/**
 * This type represents a subgraph for an attribute node that corresponds to a Managed Type.
 * <p>
 * Using this class, an entity subgraph can be embedded within an EntityGraph.
 *
 * @param <T> The type of the attribute
 */
public interface Subgraph<T> {

    /**
     * Adds one or more attribute nodes to the entity graph.
     *
     * @param attribute Attribute to add
     * @throws IllegalStateException If this {@code EntityGraph} has been statically defined
     */
    void addAttributeNodes(Attribute<T, ?>... attribute);

    /**
     * Adds one or more attribute nodes to the entity graph.
     *
     * @param attributeName Name of the attribute to add
     * @throws IllegalStateException    If this {@code EntityGraph} has been statically defined
     * @throws IllegalArgumentException If the attribute is not an attribute of this managed type
     */
    void addAttributeNodes(String... attributeName);

    /**
     * Adds a node to the graph that corresponds to a managed type.
     * <p>
     * This allows for construction of multi-node entity graphs that include related managed types.
     *
     * @param attribute Attribute to add
     * @param <X>       Attribute type
     * @return Subgraph for the attribute
     * @throws IllegalStateException    If this {@code EntityGraph} has been statically defined
     * @throws IllegalArgumentException If the attribute's target type is not a managed type
     */
    <X> Subgraph<X> addSubgraph(Attribute<T, X> attribute);

    /**
     * Adds a node to the graph that corresponds to a managed type.
     * <p>
     * This allows for construction of multi-node entity graphs that include related managed types.
     *
     * @param attributeName Name of the attribute to add
     * @param <X>           Attribute type
     * @return Subgraph for the attribute
     * @throws IllegalStateException    If this {@code EntityGraph} has been statically defined
     * @throws IllegalArgumentException If the attribute is not an attribute of this managed type
     * @throws IllegalArgumentException If the attribute's target type is not a managed type
     */
    <X> Subgraph<X> addSubgraph(String attributeName);

    /**
     * Adds a node to the graph that corresponds to a managed type with inheritance.
     * <p>
     * This allows for multiple subclass subgraphs to be defined for this node of the entity graph. Subclass subgraphs
     * will automatically include the specified attributes of superclass subgraphs.
     *
     * @param attribute Attribute to add
     * @param cls       Entity subclass
     * @param <X>       Attribute type
     * @return Subgraph for the attribute
     * @throws IllegalStateException    If this {@code EntityGraph} has been statically defined
     * @throws IllegalArgumentException If the attribute's target type is not a managed type
     */
    <X> Subgraph<? extends X> addSubgraph(Attribute<T, X> attribute, Class<? extends X> cls);

    /**
     * Gets the attribute nodes corresponding to the attributes of this managed type that are included in the subgraph.
     *
     * @return List of attribute nodes included in the subgraph or empty list if none have been defined
     */
    List<AttributeNode<?>> getAttributeNodes();

    /**
     * Gets the type for which this subgraph was defined.
     *
     * @return managed type referenced by the subgraph
     */
    Class<T> getClassType();
}

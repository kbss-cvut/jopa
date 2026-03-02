package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;

import java.util.List;

/**
 * This type represents the root of an entity graph that will be used as a template to define the attribute nodes and
 * boundaries of a graph of entities and entity relationships.
 * <p>
 * The root must be an entity type.
 * <p>
 * The methods to add subgraphs implicitly create the corresponding attribute nodes as well; such attribute nodes should
 * not be redundantly specified.
 *
 * @param <T> The type of the root entity
 */
public interface EntityGraph<T> {

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
     * Add additional attributes to this entity graph that correspond to attributes of subclasses of this EntityGraph's
     * entity type.
     * <p>
     * Subclass subgraphs will automatically include the specified attributes of superclass subgraphs.
     *
     * @param cls Entity subclass
     * @return Subgraph for the subclass
     * @throws IllegalStateException    If this {@code EntityGraph} has been statically defined
     * @throws IllegalArgumentException If the type is not a managed type
     */
    Subgraph<? extends T> addSubclassSubgraph(Class<? extends T> cls);

    /**
     * Gets the attribute nodes corresponding to the attributes of this managed type that are included in the subgraph.
     *
     * @return List of attribute nodes included in the subgraph or empty list if none have been defined
     */
    List<AttributeNode<?>> getAttributeNodes();

    /**
     * Gets the name of a named EntityGraph (an entity graph defined by means of the NamedEntityGraph annotation, or
     * added by means of the addNamedEntityGraph method).
     * <p>
     * Returns null if the EntityGraph is not a named EntityGraph.
     *
     * @return Entity graph name, {@code null} if it is not named
     */
    String getName();
}

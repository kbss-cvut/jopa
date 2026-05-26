package cz.cvut.kbss.jopa.model.annotations;

import cz.cvut.kbss.jopa.model.EntityManager;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines a named entity graph.
 * <p>
 * This annotation must be applied to the root entity of the graph, and specifies the limits of the graph of associated
 * attributes and entities fetched when an operation which retrieves an instance or instances of the root entity is
 * executed.
 * <p>
 * A reference to a named entity graph may be obtained by calling {@link EntityManager#getEntityGraph(String)}, and may
 * be passed as hint to {@link cz.cvut.kbss.jopa.model.query.TypedQuery}.
 */
@Target(value = ElementType.TYPE)
@Retention(value = RetentionPolicy.RUNTIME)
public @interface NamedEntityGraph {

    /**
     * (Optional) The name used to identify the entity graph in calls to {@link EntityManager#getEntityGraph(String)}.
     * <p>
     * If no name is explicitly specified, the name defaults to the entity name of the annotated root entity. Entity
     * graph names must be unique within the persistence unit.
     */
    String name();

    /**
     * (Optional) A list of attributes of the entity that are included in this graph.
     */
    NamedAttributeNode[] attributeNodes() default {};

    /**
     * (Optional) Includes all the attributes of the annotated entity class as attribute nodes in the
     * {@code NamedEntityGraph} without the need to explicitly list them.
     * <p>
     * Included attributes can still be fully specified by an attribute node referencing a subgraph.
     */
    boolean includeAllAttributes() default false;

    /**
     * (Optional) A list of subgraphs that are included in the entity graph.
     * <p>
     * These are referenced by name from {@code NamedAttributeNode} definitions.
     */
    NamedSubgraph[] subgraphs() default {};
}

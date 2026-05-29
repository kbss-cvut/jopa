package cz.cvut.kbss.jopa.model.annotations;

/**
 * A NamedSubgraph is a member element of a {@link NamedEntityGraph}.
 * <p>
 * The NamedSubgraph is only referenced from within its containing {@code NamedEntityGraph} and cannot be referenced
 * independently. It is referenced by its {@link #name()} from a {@link NamedAttributeNode} element of the
 * {@code NamedEntityGraph}.
 */
public @interface NamedSubgraph {

    /**
     * (Required) The name of the subgraph as referenced from a NamedAttributeNode element.
     */
    String name();

    /**
     * (Required) The list of the attributes of the class that must be included.
     * <p>
     * If the named subgraph corresponds to a subclass of the class referenced by the corresponding attribute node, then
     * only subclass-specific attributes are listed.
     */
    NamedAttributeNode[] attributeNodes();

    /**
     * (Optional) The type represented by this subgraph.
     * <p>
     * The element must be specified when this subgraph is extending a definition on behalf of a subclass.
     */
    Class<?> type() default void.class;
}

package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * A NamedAttributeNode is a member element of a {@link NamedEntityGraph}.
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface NamedAttributeNode {

    /**
     * (Required) The name of the attribute that must be included in the graph.
     */
    String value();

    /**
     * (Optional) If the attribute references a managed type that has its own AttributeNodes, this element is used to
     * refer to that {@link NamedSubgraph} definition.
     * <p>
     * If the target type has inheritance, multiple subgraphs can be specified.These additional subgraphs are intended
     * to add subclass-specific attributes. Superclass subgraph entries will be merged into subclass subgraphs.
     * <p>
     * The value of this element is the name of the subgraph as specified by the name element of the corresponding
     * NamedSubgraph element. If multiple subgraphs are specified due to inheritance, they are referenced by this name.
     */
    String subgraph() default "";
}

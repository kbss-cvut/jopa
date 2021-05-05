package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.*;

/**
 * This annotation can be used for creating SPARQL queries directly on repository fields.
 *
 * <p>
 *
 * The field and query parameters cannot be of a primitive type.
 *
 * <p>
 *
 * Example:
 * <pre>
 *     <code>
 * &#64;Sparql("PREFIX jopa:&lt;http://krizik.felk.cvut.cz/ontologies/jopa/&gt;\n" +
 *             "SELECT ?stringAttribute WHERE {" +
 *             "$this jopa:attributes#B-stringAttribute ?stringAttribute}")
 *        private String stringQueryAttribute;
 *     </code>
 * </pre>
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Sparql {

    /**
     * SPARQL query including any prefixes and parameters.
     */
    String query();

    /**
     * If {@code FetchType.LAZY} is specified the attribute will not be initialized during entity construction
     * but rather only when it is required by a getter method.
     */
    FetchType fetchType() default FetchType.EAGER;
}

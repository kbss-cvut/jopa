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
 * Example on method: //TODO change to field
 * <pre>
 *     <code>
 * &#64;Sparql("PREFIX foo:&lt;http://example.com/resources/&gt;\n" +
 *                "SELECT ?person WHERE {?person foo:name $name}")
 *        public Person findPersonByName(@Bind("name") String name) {
 *            return null;
 *        }
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
}

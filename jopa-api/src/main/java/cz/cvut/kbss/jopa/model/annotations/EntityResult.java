package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Used in conjunction with the {@link SparqlResultSetMapping} annotation to map the SELECT clause of a SPARQL query to
 * an entity result.
 * <p>
 * If this annotation is used, the SPARQL statement should select all of the variables that are mapped to the entity
 * object. This should include attributes of related entities. The results obtained when insufficient data is available
 * are undefined.
 * <p>
 * Example:
 * <pre>
 *     <code>
 * Query q = em.createNativeQuery(
 * "SELECT ?uri, ?quantity, ?item, ?label, ?description "+
 * "WHERE {
 *     ?uri a &lt;http://onto.fel.cvut.cz/ontologies/jopa/Order&gt; ;
 *        &lt;http://onto.fel.cvut.cz/ontologies/ufo/has_part&gt ?item .
 *     ?item a &lt;http://onto.fel.cvut.cz/ontologies/jopa/Order&gt; ;
 *           rdfs:label ?label;
 *           rdfs:comment ?description .
 * }", "OrderItemResults");
 * {@literal @}SparqlResultSetMapping(name="OrderItemResults",
 * entities={
 * {@literal @}EntityResult(entityClass=cz.cvut.kbss.jopa.Order.class),
 * {@literal @}EntityResult(entityClass=cz.cvut.kbss.jopa.Item.class)
 * })
 *     </code>
 * </pre>
 *
 * @see SparqlResultSetMapping
 */
@Target({})
@Retention(value = RetentionPolicy.RUNTIME)
public @interface EntityResult {

    /**
     * The class of the result.
     */
    Class<?> entityClass();

    /**
     * Maps the variables specified in the SELECT list of the query to the properties or fields of the entity class.
     */
    FieldResult[] fields() default {};
}

package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Used in conjunction with the {@link SparqlResultSetMapping} annotation or {@link ConstructorResult} annotation to map
 * a column of the SELECT list of a SPARQL query.
 * <p>
 * The name element references the name of a variable in the SELECT list. Scalar result types can be included in the
 * query result by specifying this annotation in the metadata.
 * <p>
 * Example:
 * <pre>
 *     <code>
 * Query q = em.createNativeQuery("SELECT ?uri ?label ?comment WHERE {" +
 *             "?uri a <http://onto.fel.cvut.cz/ontologies/jopa/Example> ;" +
 *                  "rdfs:label ?label ;" +
 *                  "rdfs:comment ?comment ." +
 *         "}", "ExampleResults");
 *     </code>
 * </pre>
 * <p>
 * <pre>
 *     <code>
 * {@literal @}SparqlResultSetMapping(name="ExampleResults",
 *          classes={
 *              {@literal @}ConstructorResult(targetClass=cz.cvut.kbss.jopa.Example, variables={
 *                  {@literal @}VariableResult(name="uri"),
 *                  {@literal @}VariableResult(name="label"),
 *                  {@literal @}VariableResult(name="comment")
 *              }
 *          }
 *      )
 *     </code>
 * </pre>
 */
@Target(value = {})
@Retention(RetentionPolicy.RUNTIME)
public @interface VariableResult {

    /**
     * (Required) The name of a variable in the SELECT clause of a SPARQL query.
     */
    String name();

    /**
     * (Optional) The Java type to which the variable mapping type is to be mapped.
     * <p>
     * If the type element is not specified, the default OntoDriver type mapping for the variable mapping will be used.
     */
    Class<?> type() default void.class;
}

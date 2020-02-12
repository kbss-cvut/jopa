/**
 * Copyright (C) 2020 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Used in conjunction with the {@link SparqlResultSetMapping} annotation or {@link ConstructorResult} annotation to map
 * a column of the SELECT list of a SPARQL query.
 * <p>
 * The name element references the name of a variable in the SELECT list. Scalar result types can be included in the
 * query result by specifying this annotation in the metadata. If the variable is not bound in the result, {@code null}
 * is returned.
 * <p>
 * Example:
 * <pre>
 *     <code>
 * Query q = em.createNativeQuery("SELECT ?uri ?label ?comment WHERE {" +
 *             "?uri a &lt;http://onto.fel.cvut.cz/ontologies/jopa/Example&gt; ;" +
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
 *
 * @see SparqlResultSetMapping
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

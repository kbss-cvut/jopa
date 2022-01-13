/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
 *        &lt;http://onto.fel.cvut.cz/ontologies/ufo/has_part&gt; ?item .
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

/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Used in conjunction with the {@link EntityResult} annotation to map columns specified in the SELECT list of a SPARQL
 * query to the properties or fields of an entity class.
 *
 * Note that if variable is not bound in result row, the field is skipped (its value is not set).
 * <p>
 * Example:
 * <pre>
 *     <code>
 *         Query q = em.createNativeQuery(
 * "SELECT ?x, ?quantity, ?item, ?label, ?description "+
 * "WHERE {
 *     ?x a &lt;http://onto.fel.cvut.cz/ontologies/jopa/Order&gt; ;
 *        &lt;http://onto.fel.cvut.cz/ontologies/ufo/has_part&gt; ?item .
 *     ?item a &lt;http://onto.fel.cvut.cz/ontologies/jopa/Order&gt; ;
 *           rdfs:label ?label;
 *           rdfs:comment ?description .
 * }", "OrderItemResults");
 *
 * {@literal @}SparqlResultSetMapping(name="OrderItemResults",
 * entities={
 * {@literal @}EntityResult(entityClass=cz.cvut.kbss.jopa.Order.class, fields = {
 *     {@literal @}FieldResult(name="uri", variable="x")
 * }),
 * {@literal @}EntityResult(entityClass=cz.cvut.kbss.jopa.Item.class)
 * })
 *     </code>
 * </pre>
 *
 * @see EntityResult
 * @see SparqlResultSetMapping
 */
@Target({})
@Retention(RetentionPolicy.RUNTIME)
public @interface FieldResult {

    /**
     * Name of the persistent field or property of the class.
     */
    String name();

    /**
     * Name of the variable in the SELECT clause.
     */
    String variable();
}

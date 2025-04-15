/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.model.query.Query;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Specifies the mapping of the result of a native SPARQL query.
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
 *
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
 * @see Query
 * @see NamedNativeQuery
 */
@Target(value = ElementType.TYPE)
@Retention(value = RetentionPolicy.RUNTIME)
public @interface SparqlResultSetMapping {

    /**
     * The name given to the result set mapping and used to refer to it in the methods of the {@link Query} API.
     */
    String name();

    /**
     * Specifies the result set mapping to constructors.
     */
    ConstructorResult[] classes() default {};

    /**
     * Specifies the result set mapping to entities.
     */
    EntityResult[] entities() default {};

    /**
     * Specifies the result set mapping to scalar values.
     */
    VariableResult[] variables() default {};
}

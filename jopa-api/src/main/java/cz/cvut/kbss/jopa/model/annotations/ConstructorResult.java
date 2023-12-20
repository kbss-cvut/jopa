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
 * Used in conjunction with the {@link SparqlResultSetMapping} annotation to map the SELECT clause of a SPARQL query to
 * a constructor.
 * <p>
 * Applies a constructor for the target class, passing in as arguments values from the specified variables. All
 * variables corresponding to arguments of the intended constructor must be specified using the {@code variables}
 * element of the {@code ConstructorResult} annotation in the same order as that of the argument list of the
 * constructor. Any entities returned as constructor results will be in either the new or detached state, depending on
 * whether a primary key is retrieved for the constructed object.
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
 * @see SparqlResultSetMapping
 * @see VariableResult
 */
@Target(value = {})
@Retention(RetentionPolicy.RUNTIME)
public @interface ConstructorResult {

    /**
     * (Required) The class whose constructor is to be invoked.
     */
    Class<?> targetClass();

    /**
     * (Required) The mapping of variables in the SELECT list to the arguments of the intended constructor, in order.
     */
    VariableResult[] variables();
}

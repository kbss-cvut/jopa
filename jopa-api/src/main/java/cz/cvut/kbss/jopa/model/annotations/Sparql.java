/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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

import java.lang.annotation.*;

/**
 * This annotation can be used for creating SPARQL queries directly on repository fields.
 * <p>
 * The field and query parameters cannot be of a primitive type.
 * <p>
 * Example:
 * <pre>
 *     <code>
 * &#64;Sparql("PREFIX jopa:&lt;http://krizik.felk.cvut.cz/ontologies/jopa/&gt;\n" +
 *             "SELECT ?stringAttribute WHERE {" +
 *             "?this jopa:attributes#B-stringAttribute ?stringAttribute}")
 *        private String stringQueryAttribute;
 *     </code>
 * </pre>
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Sparql {

    /**
     * A native SPARQL query including any prefixes and parameters.
     */
    String query();

    /**
     * Whether to allow referencing other entity attributes in the query.
     * <p>
     * If enabled, the provider will attempt to replace variables with names matching entity attributes with the
     * attribute values.
     * <p>
     * Note that lazily loaded attribute values may not be used.
     *
     * @return {@code true} if variables referencing other entity attributes should be replaced in the query with the
     * attribute values, {@code false} otherwise
     */
    boolean enableReferencingAttributes() default true;

    /**
     * If {@code FetchType.LAZY} is specified the attribute will not be initialized during entity construction but
     * rather only when it is required by a getter method.
     */
    FetchType fetchType() default FetchType.EAGER;
}

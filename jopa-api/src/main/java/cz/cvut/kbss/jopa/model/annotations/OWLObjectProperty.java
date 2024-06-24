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

import cz.cvut.kbss.jopa.model.annotations.util.Property;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Marks an attribute mapped to an OWL object property.
 *
 * The Java type of such attributes is either another entity or a valid identifier type.
 *
 * Note that for use with RDF(S), attributes annotated with this annotation are expected to reference other RDF resources.
 */
@Documented
@Property
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface OWLObjectProperty {

    /**
     * IRI of the object property
     *
     * @return IRI of the object property
     */
    String iri();

    /**
     * (Optional) The operations that must be cascaded to the target of the association.
     * <p>
     * By default no operations are cascaded.
     *
     * @return Cascading setting for the annotated attribute
     */
    CascadeType[] cascade() default {};

    /**
     * (Optional) Whether the association should be lazily loaded or must be eagerly fetched.
     *
     * @return Whether this property is read only
     */
    FetchType fetch() default FetchType.LAZY;
}

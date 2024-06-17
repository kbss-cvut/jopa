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
 * Marks an attribute mapped to an OWL datatype property.
 * <p>
 * This means that literal values are expected for such attributes.
 * <p>
 * Note that for use with RDF(S), attributes annotated with this annotation are expected to reference literals.
 */
@Documented
@Property
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface OWLDataProperty{

    /**
     * IRI of the data property
     *
     * @return IRI of the data property
     */
    String iri();

    /**
     * (Optional) Whether the association should be lazily loaded or must be eagerly fetched.
     *
     * @return Fetch type of this property
     */
    FetchType fetch() default FetchType.EAGER;

    /**
     * (Optional) Marks an attribute whose value is a lexical form of a literal value.
     * <p>
     * This parameter should be used on {@code String} attributes, as literal lexical form is always a string. Lexical
     * form of a literal of any datatype can be loaded.
     * <p>
     * Saving the lexical form is forbidden to prevent unintentional change of the data type, unless {@link #datatype()}
     * is explicitly specified.
     */
    boolean lexicalForm() default false;

    /**
     * (Optional) Whether the value should be stored as a <a href="https://www.w3.org/TR/rdf11-concepts/">simple
     * literal</a>, i.e. {@code xsd:string}.
     *
     * @return Whether the mapped value is a simple literal
     */
    boolean simpleLiteral() default false;

    /**
     * IRI of the datatype to use when storing values of this property.
     * <p>
     * If specified, the value of the attribute is treated as the lexical form of the literal (and should be a {@code String}).
     *
     * @return Datatype IRI
     */
    String datatype() default "";
}

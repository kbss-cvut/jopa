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

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import cz.cvut.kbss.jopa.model.SequencesVocabulary;

/**
 * Specifies mapping of a sequence of items.
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD, ElementType.METHOD})
public @interface Sequence {

    /**
     * Defines the type of the sequence.
     *
     * @return Type of sequence
     */
    SequenceType type() default SequenceType.referenced;

    /**
     * IRI of the class that represents the 'OWLList' concept.
     * <p>
     * Relevant only for {@link SequenceType#referenced} type.
     *
     * @return OWLList class IRI
     */
    String listClassIRI() default SequencesVocabulary.s_c_OWLList;

    /**
     * IRI of the object property that represents the 'hasContents' role.
     * <p>
     * Relevant only for {@link SequenceType#referenced} type.
     *
     * @return hasContents property IRI
     */
    String hasContentsPropertyIRI() default SequencesVocabulary.s_p_hasContents;

    /**
     * URI of the object property that represents the 'hasNext' role.
     *
     * @return hasNext property IRI
     */
    String hasNextPropertyIRI() default SequencesVocabulary.s_p_hasNext;
}

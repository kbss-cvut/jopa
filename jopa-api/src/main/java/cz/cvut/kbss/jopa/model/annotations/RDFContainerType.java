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

/**
 * Types of RDF containers, as defined by <a href="https://www.w3.org/TR/rdf12-schema/#ch_containervocab">Section
 * 5.1</a> of the RDF Schema vocabulary.
 */
public enum RDFContainerType {

    /**
     * The {@literal rdf:Alt} class is the class of RDF 'Alternative' containers.
     * <p>
     * While not formalized, it is conventionally used to indicate that the container is a list of alternatives and only
     * one of the will be used.
     */
    ALT,
    /**
     * The {@literal rdf:Bag} class is the class of RDF 'Bag' containers.
     * <p>
     * While not formalized, it is conventionally used to indicate that the container is unordered.
     */
    BAG,
    /**
     * The {@literal rdf:Seq} class is the class of RDF 'Sequence' containers.
     * <p>
     * While not formalized, it is conventionally used to indicate that the numerical ordering of the container
     * membership properties is intended to be significant.
     */
    SEQ
}

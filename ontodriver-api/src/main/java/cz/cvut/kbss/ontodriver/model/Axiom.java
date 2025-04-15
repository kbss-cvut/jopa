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
package cz.cvut.kbss.ontodriver.model;

/**
 * Represents either an OWL axiom or a RDF triple.
 */
public interface Axiom<T> {

    /**
     * Gets the subject of this axiom.
     *
     * @return Subject named resource
     */
    NamedResource getSubject();

    /**
     * Gets the assertion of this axiom.
     *
     * @return Assertion named resource
     */
    Assertion getAssertion();

    /**
     * Gets the value of this axiom.
     *
     * @return Value, either a NamedResource, or a literal value
     */
    Value<T> getValue();
}

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
package cz.cvut.kbss.jopa.model;

import java.net.URI;
import java.util.Objects;

public class IRI {

    private final String value;

    private IRI(String iri) {
        this.value = Objects.requireNonNull(iri);
    }

    public URI toURI() {
        return URI.create(value);
    }

    @Override
    public String toString() {
        return value;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof IRI iri)) {
            return false;
        }
        return value.equals(iri.value);
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }

    public static IRI create(final String s) {
        return new IRI(s);
    }
}

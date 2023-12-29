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
package cz.cvut.kbss.ontodriver.model;

import java.io.Serializable;
import java.net.URI;
import java.util.Objects;

/**
 * Represents named resources, i.e. resources identified by a URI.
 */
public class NamedResource implements Serializable {

    private static final long serialVersionUID = 5932515448919851871L;

    private final URI identifier;

    NamedResource(URI uri) {
        this.identifier = Objects.requireNonNull(uri);
    }

    /**
     * Gets the identifier of this resource.
     *
     * @return URI
     */
    public URI getIdentifier() {
        return identifier;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + identifier.hashCode();
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        NamedResource other = (NamedResource) obj;
        return identifier.equals(other.identifier);
    }

    @Override
    public String toString() {
        return identifier.toString();
    }

    /**
     * Creates new named resource from the specified URI.
     *
     * @param uri Resource identifier
     * @return NamedResource instance
     */
    public static NamedResource create(URI uri) {
        return new NamedResource(uri);
    }

    /**
     * Creates new named resource from the specified string identifier.
     *
     * @param iri Resource identifier
     * @return NamedResource instance
     */
    public static NamedResource create(String iri) {
        return new NamedResource(URI.create(iri));
    }
}

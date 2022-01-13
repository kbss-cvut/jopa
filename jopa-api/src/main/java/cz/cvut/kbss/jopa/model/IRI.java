/**
 * Copyright (C) 2022 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model;

import java.net.URI;
import java.util.Objects;

public class IRI implements AnnotationValue {

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
        if (!(o instanceof IRI)) {
            return false;
        }
        IRI iri = (IRI) o;
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

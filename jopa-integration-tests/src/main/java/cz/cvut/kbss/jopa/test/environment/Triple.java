/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.test.environment;

import java.net.URI;
import java.util.Objects;

public class Triple {

    private URI subject;
    private URI property;
    private Object value;

    public Triple(URI subject, URI property, Object value) {
        this.subject = Objects.requireNonNull(subject);
        this.property = Objects.requireNonNull(property);
        this.value = Objects.requireNonNull(value);
    }

    public URI getSubject() {
        return subject;
    }

    public URI getProperty() {
        return property;
    }

    public Object getValue() {
        return value;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Triple)) return false;

        Triple triple = (Triple) o;

        if (!subject.equals(triple.subject)) return false;
        if (!property.equals(triple.property)) return false;
        return value.equals(triple.value);

    }

    @Override
    public int hashCode() {
        int result = subject.hashCode();
        result = 31 * result + property.hashCode();
        result = 31 * result + value.hashCode();
        return result;
    }
}

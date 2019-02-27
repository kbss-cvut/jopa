/**
 * Copyright (C) 2019 Czech Technical University in Prague
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

    private final URI subject;
    private final URI property;
    private final Object value;
    private final String language;

    public Triple(URI subject, URI property, Object value) {
        this.subject = Objects.requireNonNull(subject);
        this.property = Objects.requireNonNull(property);
        this.value = Objects.requireNonNull(value);
        this.language = "en";
    }

    public Triple(URI subject, URI property, Object value, String language) {
        this.subject = Objects.requireNonNull(subject);
        this.property = Objects.requireNonNull(property);
        this.value = Objects.requireNonNull(value);
        this.language = language;
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

    public String getLanguage() {
        return language;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Triple)) return false;

        Triple triple = (Triple) o;

        return subject.equals(triple.subject) && property.equals(triple.property) && language.equals(triple.language) &&
                value.equals(triple.value);

    }

    @Override
    public int hashCode() {
        int result = subject.hashCode();
        result = 31 * result + property.hashCode();
        result = 31 * result + value.hashCode();
        result = 31 * result + language.hashCode();
        return result;
    }
}

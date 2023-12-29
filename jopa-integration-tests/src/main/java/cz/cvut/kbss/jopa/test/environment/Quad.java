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
package cz.cvut.kbss.jopa.test.environment;

import java.net.URI;
import java.util.Objects;

public class Quad {

    private final URI subject;
    private final URI property;
    private final Object value;
    private final URI context;
    private final String language;

    public Quad(URI subject, URI property, Object value) {
        this.subject = Objects.requireNonNull(subject);
        this.property = Objects.requireNonNull(property);
        this.value = Objects.requireNonNull(value);
        this.context = null;
        this.language = "en";
    }

    public Quad(URI subject, URI property, Object value, URI context) {
        this.subject = Objects.requireNonNull(subject);
        this.property = Objects.requireNonNull(property);
        this.value = Objects.requireNonNull(value);
        this.context = context;
        this.language = "en";
    }

    public Quad(URI subject, URI property, Object value, String language) {
        this.subject = Objects.requireNonNull(subject);
        this.property = Objects.requireNonNull(property);
        this.value = Objects.requireNonNull(value);
        this.context = null;
        this.language = language;
    }

    public Quad(URI subject, URI property, Object value, String language, URI context) {
        this.subject = Objects.requireNonNull(subject);
        this.property = Objects.requireNonNull(property);
        this.value = Objects.requireNonNull(value);
        this.context = context;
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

    public URI getContext() {
        return context;
    }

    public String getLanguage() {
        return language;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof Quad)) {
            return false;
        }
        Quad quad = (Quad) o;
        return subject.equals(quad.subject) &&
                property.equals(quad.property) &&
                value.equals(quad.value) &&
                Objects.equals(context, quad.context) &&
                Objects.equals(language, quad.language);
    }

    @Override
    public int hashCode() {
        return Objects.hash(subject, property, value, context, language);
    }

    @Override
    public String toString() {
        return "{" + subject + " " + property + " " + value +
                (language != null ? "@" + language : "") +
                (context != null ? " " + context : "") + "}";
    }
}

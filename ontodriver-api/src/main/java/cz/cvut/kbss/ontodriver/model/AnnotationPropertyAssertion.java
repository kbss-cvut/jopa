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

import java.net.URI;

final class AnnotationPropertyAssertion extends Assertion {

    AnnotationPropertyAssertion(URI assertionIdentifier, boolean isInferred) {
        super(assertionIdentifier, isInferred);
    }

    AnnotationPropertyAssertion(URI assertionIdentifier, String language, boolean isInferred) {
        super(assertionIdentifier, language, isInferred);
    }

    @Override
    public AssertionType getType() {
        return AssertionType.ANNOTATION_PROPERTY;
    }

    @Override
    public int hashCode() {
        int prime = 31;
        return prime * super.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        return this == obj || super.equals(obj) && getClass() == obj.getClass();
    }

    @Override
    public String toString() {
        return super.toString() + (language != null ? " @" + language : "");
    }
}

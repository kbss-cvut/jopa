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
package cz.cvut.kbss.ontodriver.model;

import java.net.URI;
import java.util.Random;

class PropertyAssertion extends Assertion {

    private static final URI UNSPECIFIED_PROPERTY = URI.create("http://" + Math.abs(new Random().nextInt(Integer.MAX_VALUE)));

    PropertyAssertion(boolean isInferred) {
        super(UNSPECIFIED_PROPERTY, isInferred);
    }

    PropertyAssertion(String language, boolean isInferred) {
        super(UNSPECIFIED_PROPERTY, language, isInferred);
    }

    PropertyAssertion(URI identifier, boolean isInferred) {
        super(identifier, isInferred);
    }

    PropertyAssertion(URI identifier, String language, boolean isInferred) {
        super(identifier, language, isInferred);
    }

    @Override
    public AssertionType getType() {
        return AssertionType.PROPERTY;
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

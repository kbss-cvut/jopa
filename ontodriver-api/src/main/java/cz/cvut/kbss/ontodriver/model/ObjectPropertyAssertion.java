/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.model;

import java.net.URI;

final class ObjectPropertyAssertion extends Assertion {

    ObjectPropertyAssertion(URI assertionIdentifier, boolean isInferred) {
        super(assertionIdentifier, isInferred);

    }

    @Override
    public AssertionType getType() {
        return AssertionType.OBJECT_PROPERTY;
    }

    /**
     * Always returns {@code null}, because language is irrelevant for object properties.
     *
     * @return {@code null}
     */
    @Override
    public String getLanguage() {
        return null;
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
}

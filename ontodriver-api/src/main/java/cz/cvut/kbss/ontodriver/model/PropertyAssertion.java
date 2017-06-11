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
import java.util.Random;

class PropertyAssertion extends Assertion {

    private static final URI UNSPECIFIED_PROPERTY = URI.create("http://" + new Random().nextInt());

    private String language;

    PropertyAssertion(boolean isInferred) {
        super(UNSPECIFIED_PROPERTY, isInferred);
    }

    PropertyAssertion(URI identifier, boolean isInferred) {
        super(identifier, isInferred);
    }

    @Override
    public AssertionType getType() {
        return AssertionType.PROPERTY;
    }

    /**
     * Gets the language tag carried by this assertion.
     * <p>
     * The language tag applies only to string-based literals.
     *
     * @return Language tag, e.g. {@code en}, can be {@code null}
     */
    public String getLanguage() {
        return language;
    }

    public void setLanguage(String language) {
        this.language = language;
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

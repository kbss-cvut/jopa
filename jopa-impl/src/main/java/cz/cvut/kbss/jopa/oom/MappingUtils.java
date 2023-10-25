/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.ontodriver.model.Assertion.AssertionType;
import cz.cvut.kbss.ontodriver.model.Axiom;

final class MappingUtils {

    private MappingUtils() {
        throw new AssertionError();
    }

    /**
     * Checks whether the specified axioms is a class assertion for an instance
     * of the specified entity type. </p>
     *
     * @param ax Axiom
     * @param et Entity type
     * @return True if the axioms asserts that an individual is of type
     * represented by the entity type
     */
    static boolean isEntityClassAssertion(Axiom<?> ax, EntityType<?> et) {
        return isClassAssertion(ax) && isEntityClass(ax, et);
    }

    /**
     * Returns true if the specified axiom is a class assertion axiom.
     *
     * @param ax Axiom to check
     * @return true if class assertion
     */
    static boolean isClassAssertion(Axiom<?> ax) {
        return ax.getAssertion().getType() == AssertionType.CLASS;
    }

    private static boolean isEntityClass(Axiom<?> ax, EntityType<?> et) {
        final String type = et.getIRI().toString();
        final String val = ax.getValue().stringValue();
        return val.equals(type);
    }
}

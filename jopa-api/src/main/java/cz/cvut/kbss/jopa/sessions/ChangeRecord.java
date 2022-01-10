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
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;

/**
 * Objects of classes implementing this interface represent a change of one attribute of an entity class. Objects store
 * only the new value, old value is in the original and it is not needed.
 */
public interface ChangeRecord {

    /**
     * Returns the new value of the attribute.
     *
     * @return Object
     */
    Object getNewValue();

    /**
     * Sets the new value of the attribute in case this change record needs to be updated.
     *
     * @param value The value to set
     */
    void setNewValue(Object value);

    /**
     * Gets the attribute to which this change record is bound.
     *
     * @return the attribute
     */
    FieldSpecification<?, ?> getAttribute();

    /**
     * Whether this change record prevents caching of the instance on which the change is applied.
     *
     * @return Whether this change record prevents caching
     */
    boolean doesPreventCaching();

    /**
     * Marks this change record to prevent caching.
     *
     * @see #doesPreventCaching()
     */
    void preventCaching();
}

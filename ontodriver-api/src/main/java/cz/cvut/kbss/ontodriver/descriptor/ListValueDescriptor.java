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
package cz.cvut.kbss.ontodriver.descriptor;

import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.util.List;

/**
 * Descriptor for list values.
 *
 */
public interface ListValueDescriptor extends ListDescriptor {

    /**
     * Gets values from the list described by this descriptor.
     *
     * @return List of value identifiers
     */
    List<NamedResource> getValues();

    /**
     * Adds value to this list descriptor.
     *
     * @param elem
     *            The value to add, i. e. identifier of the list element
     */
    void addValue(NamedResource elem);
}

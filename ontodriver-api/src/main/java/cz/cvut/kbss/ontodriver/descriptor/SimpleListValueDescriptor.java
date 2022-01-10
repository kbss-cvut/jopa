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
package cz.cvut.kbss.ontodriver.descriptor;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * Represents values of a simple sequence.
 *
 * @see SimpleListDescriptorImpl
 */
public class SimpleListValueDescriptor extends SimpleListDescriptorImpl implements
        ListValueDescriptor {

    private final List<NamedResource> values;

    public SimpleListValueDescriptor(NamedResource listOwner, Assertion listProperty,
                                     Assertion nextNodeProperty) {
        super(listOwner, listProperty, nextNodeProperty);
        this.values = new ArrayList<>();
    }

    @Override
    public List<NamedResource> getValues() {
        return Collections.unmodifiableList(values);
    }

    @Override
    public void addValue(NamedResource value) {
        Objects.requireNonNull(value);
        values.add(value);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + values.hashCode();
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null || getClass() != obj.getClass())
            return false;
        SimpleListValueDescriptor other = (SimpleListValueDescriptor) obj;
        return descriptor.equals(other.descriptor) && values.equals(other.values);
    }

    @Override
    public String toString() {
        return "[SimpleListValueDescriptor: owner = " + descriptor.getListOwner() + ", values = " + values + "]";
    }
}

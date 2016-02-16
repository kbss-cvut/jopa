/**
 * Copyright (C) 2011 Czech Technical University in Prague
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

public class ReferencedListValueDescriptor extends ReferencedListDescriptorImpl implements
        ListValueDescriptor {

    private final List<NamedResource> values;

    public ReferencedListValueDescriptor(NamedResource listOwner, Assertion listProperty,
                                         Assertion nextNode, Assertion nodeContent) {
        super(listOwner, listProperty, nextNode, nodeContent);
        this.values = new ArrayList<>();
    }

    @Override
    public List<NamedResource> getValues() {
        return Collections.unmodifiableList(values);
    }

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
        if (getClass() != obj.getClass())
            return false;
        ReferencedListValueDescriptor other = (ReferencedListValueDescriptor) obj;
        if (!descriptor.equals(other.descriptor))
            return false;
        if (!getNodeContent().equals(other.getNodeContent()))
            return false;
        if (!values.equals(other.values))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "[ReferencedListValueDescriptor: owner = " + descriptor.getListOwner() + ", values = " + values + "]";
    }
}

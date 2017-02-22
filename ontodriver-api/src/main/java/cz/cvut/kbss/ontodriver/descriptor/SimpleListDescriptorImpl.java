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

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;

/**
 * Describes a simple sequence.
 * <p>
 * Simple lists are classic Lips-style lists (singly-linked lists), where each node is a subject for axiom referencing
 * the next node.
 */
public class SimpleListDescriptorImpl implements SimpleListDescriptor {

    protected final ListDescriptor descriptor;

    public SimpleListDescriptorImpl(NamedResource listOwner, Assertion listProperty,
                                    Assertion nextNodeProperty) {
        this.descriptor = new BaseListDescriptorImpl(listOwner, listProperty, nextNodeProperty);
    }

    @Override
    public void setContext(URI context) {
        descriptor.setContext(context);
    }

    @Override
    public URI getContext() {
        return descriptor.getContext();
    }

    @Override
    public NamedResource getListOwner() {
        return descriptor.getListOwner();
    }

    @Override
    public Assertion getListProperty() {
        return descriptor.getListProperty();
    }

    @Override
    public Assertion getNextNode() {
        return descriptor.getNextNode();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + descriptor.hashCode();
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        SimpleListDescriptorImpl other = (SimpleListDescriptorImpl) obj;
        return descriptor.equals(other.descriptor);
    }

    @Override
    public String toString() {
        return "[SimpleList: " + descriptor + "]";
    }
}

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

import java.net.URI;
import java.util.Objects;

/**
 * Represents singly-linked referenced list. </p>
 * <p>
 * In referenced list each node has content linked to it by a separate property. In addition, each node points to its
 * successor in the sequence (with the exception of the last node, which has no successor).
 *
 * @author ledvima1
 */
public class ReferencedListDescriptorImpl implements ReferencedListDescriptor {

    protected final ListDescriptor descriptor;
    private final Assertion nodeContent;

    public ReferencedListDescriptorImpl(NamedResource listOwner, Assertion listProperty,
                                        Assertion nextNode, Assertion nodeContent) {
        this.descriptor = new BaseListDescriptorImpl(listOwner, listProperty, nextNode);
        this.nodeContent = Objects.requireNonNull(nodeContent);
    }

    @Override
    public URI getContext() {
        return descriptor.getContext();
    }

    @Override
    public void setContext(URI context) {
        descriptor.setContext(context);
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
    public Assertion getNodeContent() {
        return nodeContent;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + descriptor.hashCode();
        result = prime * result + nodeContent.hashCode();
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
        ReferencedListDescriptorImpl other = (ReferencedListDescriptorImpl) obj;
        if (!descriptor.equals(other.descriptor))
            return false;
        if (!nodeContent.equals(other.nodeContent))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "[ReferencedList: " + descriptor + ", nodeContent = " + nodeContent + "]";
    }
}

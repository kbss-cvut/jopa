/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.descriptor;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;
import java.util.Objects;

/**
 * Represents singly-linked referenced list.
 * <p>
 * In referenced list each node has content linked to it by a separate property. In addition, each node points to its
 * successor in the sequence. The last node may point to {@literal rdf:nil} or it may just lack a successor.
 */
public class ReferencedListDescriptorImpl implements ReferencedListDescriptor {

    protected final ListDescriptor descriptor;
    private final Assertion nodeContent;

    private final boolean terminatedByNil;

    public ReferencedListDescriptorImpl(NamedResource listOwner, Assertion listProperty,
                                        Assertion nextNode, Assertion nodeContent) {
        this.descriptor = new BaseListDescriptorImpl(listOwner, listProperty, nextNode);
        this.nodeContent = Objects.requireNonNull(nodeContent);
        this.terminatedByNil = false;
    }

    public ReferencedListDescriptorImpl(NamedResource listOwner, Assertion listProperty,
                                        Assertion nextNode, Assertion nodeContent, boolean terminatedByNil) {
        this.descriptor = new BaseListDescriptorImpl(listOwner, listProperty, nextNode);
        this.nodeContent = Objects.requireNonNull(nodeContent);
        this.terminatedByNil = terminatedByNil;
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
    public boolean isTerminatedByNil() {
        return terminatedByNil;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + descriptor.hashCode();
        result = prime * result + nodeContent.hashCode();
        result = prime * result + Boolean.hashCode(terminatedByNil);
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        ReferencedListDescriptorImpl other = (ReferencedListDescriptorImpl) obj;
        return descriptor.equals(other.descriptor) && nodeContent.equals(other.nodeContent) && terminatedByNil == other.terminatedByNil;
    }

    @Override
    public String toString() {
        return "[ReferencedList: " + descriptor + ", nodeContent = " + nodeContent + "]";
    }
}

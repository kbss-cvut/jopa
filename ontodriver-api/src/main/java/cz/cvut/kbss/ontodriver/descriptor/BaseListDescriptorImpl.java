/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
import cz.cvut.kbss.ontodriver.util.ErrorUtils;

import java.net.URI;
import java.util.Objects;

final class BaseListDescriptorImpl implements ListDescriptor {

    private final NamedResource listOwner;
    private final Assertion listProperty;
    private final Assertion nextNode;

    private URI context;

    public BaseListDescriptorImpl(NamedResource listOwner, Assertion listProperty, Assertion nextNode) {
        this.listOwner = Objects.requireNonNull(listOwner, ErrorUtils.getNPXMessageSupplier("listOwner"));
        this.listProperty = Objects.requireNonNull(listProperty, ErrorUtils.getNPXMessageSupplier("listProperty"));
        this.nextNode = Objects.requireNonNull(nextNode, ErrorUtils.getNPXMessageSupplier("nextNode"));
    }

    @Override
    public URI getContext() {
        return context;
    }

    @Override
    public void setContext(URI context) {
        // null permitted here
        this.context = context;
    }

    @Override
    public NamedResource getListOwner() {
        return listOwner;
    }

    @Override
    public Assertion getListProperty() {
        return listProperty;
    }

    @Override
    public Assertion getNextNode() {
        return nextNode;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + listOwner.hashCode();
        result = prime * result + listProperty.hashCode();
        result = prime * result + nextNode.hashCode();
        result = prime * result + ((context == null) ? 0 : context.hashCode());
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
        BaseListDescriptorImpl other = (BaseListDescriptorImpl) obj;
        if (!listOwner.equals(other.listOwner))
            return false;
        if (!listProperty.equals(other.listProperty))
            return false;
        if (!nextNode.equals(other.nextNode))
            return false;
        if (context == null) {
            return other.context == null;
        } else return context.equals(other.context);
    }

    @Override
    public String toString() {
        return "owner = " + listOwner + ", list = " + listProperty + ", next node = " + nextNode + ", context = " +
                context;
    }
}

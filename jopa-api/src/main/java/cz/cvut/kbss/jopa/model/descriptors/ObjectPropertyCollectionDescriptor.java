/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.descriptors;

import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Set;

public class ObjectPropertyCollectionDescriptor extends FieldDescriptor {

    private final EntityDescriptor elementDescriptor;

    public ObjectPropertyCollectionDescriptor(Field attribute) {
        super(attribute);
        this.elementDescriptor = new EntityDescriptor();
    }

    public ObjectPropertyCollectionDescriptor(URI context, Field attribute) {
        super(context, attribute);
        this.elementDescriptor = new EntityDescriptor(context);
    }

    @Override
    public Descriptor getAttributeDescriptor(FieldSpecification<?, ?> attribute) {
        if (getField().equals(attribute.getJavaField())) {
            return this;
        }
        return elementDescriptor.getAttributeDescriptor(attribute);
    }

    @Override
    public void addAttributeDescriptor(Field attribute, Descriptor descriptor) {
        elementDescriptor.addAttributeDescriptor(attribute, descriptor);
    }

    @Override
    public void addAttributeContext(Field attribute, URI context) {
        elementDescriptor.addAttributeContext(attribute, context);
    }

    @Override
    protected Set<URI> getContextsInternal(Set<URI> contexts, Set<Descriptor> visited) {
        if (visited.contains(this)) {
            return contexts;
        }
        if (contexts == null) {
            return null;
        }
        visited.add(this);
        contexts.add(context);
        return elementDescriptor.getContextsInternal(contexts, visited);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((elementDescriptor == null) ? 0 : elementDescriptor.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        ObjectPropertyCollectionDescriptor other = (ObjectPropertyCollectionDescriptor) obj;
        if (elementDescriptor == null) {
            return other.elementDescriptor == null;
        } else return elementDescriptor.equals(other.elementDescriptor);
    }
}

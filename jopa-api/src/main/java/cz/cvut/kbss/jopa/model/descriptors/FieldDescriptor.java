/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
import java.util.Collection;
import java.util.Collections;
import java.util.Objects;
import java.util.Set;

/**
 * Describes a singular data property or a plural data, object or annotation property field.
 */
public class FieldDescriptor extends Descriptor {

    private final Field field;

    public FieldDescriptor(Field attribute) {
        this.field = Objects.requireNonNull(attribute);
    }

    public FieldDescriptor(URI context, Field attribute) {
        super(context);
        this.field = Objects.requireNonNull(attribute);
    }

    @Override
    public Collection<Descriptor> getAttributeDescriptors() {
        return Collections.singleton(this);
    }

    @Override
    public Descriptor getAttributeDescriptor(FieldSpecification<?, ?> attribute) {
        return getFieldDescriptor(attribute.getJavaField());
    }

    @Override
    public void addAttributeDescriptor(Field attribute, Descriptor descriptor) {
        // Do nothing
    }

    @Override
    public void addAttributeContext(Field attribute, URI context) {
        // Do nothing
    }

    /**
     * Use {@link #setLanguage(String)} instead.
     */
    @Override
    public void setAttributeLanguage(Field attribute, String languageTag) {
        // Do nothing
    }

    private Descriptor getFieldDescriptor(Field field) {
        if (this.field.equals(field)) {
            return this;
        }
        throw new IllegalArgumentException("This field descriptor does not describe field " + field);
    }

    @Override
    protected Set<URI> getContextsInternal(Set<URI> contexts, Set<Descriptor> visited) {
        if (context == null) {
            return null;
        }
        contexts.add(context);
        visited.add(this);
        return contexts;
    }

    Field getField() {
        return field;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + field.hashCode();
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
        FieldDescriptor other = (FieldDescriptor) obj;
        return field.equals(other.field);
    }
}

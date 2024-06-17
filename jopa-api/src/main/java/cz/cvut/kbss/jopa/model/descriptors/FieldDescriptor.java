/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
public class FieldDescriptor extends AbstractDescriptor {

    private final Field field;

    public FieldDescriptor(FieldSpecification<?, ?> attribute) {
        this.field = Objects.requireNonNull(attribute).getJavaField();
    }

    public FieldDescriptor(URI context, FieldSpecification<?, ?> attribute) {
        super(context);
        this.field = Objects.requireNonNull(attribute).getJavaField();
    }

    public FieldDescriptor(Set<URI> contexts, FieldSpecification<?, ?> attribute) {
        this(attribute);
        this.contexts.addAll(Objects.requireNonNull(contexts));
    }

    @Override
    public Collection<Descriptor> getAttributeDescriptors() {
        return Collections.singleton(this);
    }

    @Override
    public Descriptor getAttributeDescriptor(FieldSpecification<?, ?> attribute) {
        Objects.requireNonNull(attribute);
        return getFieldDescriptor(attribute.getJavaField());
    }

    @Override
    public Set<URI> getAttributeContexts(FieldSpecification<?, ?> attribute) {
        Objects.requireNonNull(attribute);
        return getFieldDescriptor(attribute.getJavaField()).getContexts();
    }

    @Override
    public FieldDescriptor addAttributeDescriptor(FieldSpecification<?, ?> attribute, Descriptor descriptor) {
        // Do nothing
        return this;
    }

    @Override
    public FieldDescriptor addAttributeContext(FieldSpecification<?, ?> attribute, URI context) {
        // Do nothing
        return this;
    }

    /**
     * Use {@link #setLanguage(String)} instead.
     */
    @Override
    public FieldDescriptor setAttributeLanguage(FieldSpecification<?, ?> attribute, String languageTag) {
        // Do nothing
        return this;
    }

    private AbstractDescriptor getFieldDescriptor(Field field) {
        if (this.field.equals(field)) {
            return this;
        }
        throw new IllegalArgumentException("This field descriptor does not describe field " + field);
    }

    Field getField() {
        return field;
    }

    @Override
    public boolean overridesAssertionContext() {
        return true;
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

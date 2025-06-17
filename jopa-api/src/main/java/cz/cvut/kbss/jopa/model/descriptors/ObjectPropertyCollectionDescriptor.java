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
package cz.cvut.kbss.jopa.model.descriptors;

import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Objects;
import java.util.Set;

/**
 * Allows providing descriptor for elements of an object property collection.
 */
public class ObjectPropertyCollectionDescriptor extends FieldDescriptor {

    private final EntityDescriptor elementDescriptor;

    public ObjectPropertyCollectionDescriptor(FieldSpecification<?, ?> attribute) {
        super(attribute);
        this.elementDescriptor = new EntityDescriptor();
    }

    public ObjectPropertyCollectionDescriptor(URI context, FieldSpecification<?, ?> attribute) {
        super(context, attribute);
        this.elementDescriptor = new EntityDescriptor(context);
    }

    public ObjectPropertyCollectionDescriptor(Set<URI> contexts, FieldSpecification<?, ?> attribute) {
        super(contexts, attribute);
        this.elementDescriptor = new EntityDescriptor(contexts);
    }

    public ObjectPropertyCollectionDescriptor(URI context, FieldSpecification<?, ?> attribute,
                                              boolean assertionsInSubjectContext) {
        super(context, attribute);
        this.elementDescriptor = new EntityDescriptor(context, assertionsInSubjectContext);
    }

    ObjectPropertyCollectionDescriptor(FieldSpecification<?, ?> attribute, EntityDescriptor elementDescriptor) {
        super(attribute);
        this.elementDescriptor = elementDescriptor;
    }

    protected ObjectPropertyCollectionDescriptor(Set<URI> contexts, boolean assertionsInSubjectContext, String language,
                                                 boolean hasLanguage, boolean includeInferred, Field field,
                                                 EntityDescriptor elementDescriptor) {
        super(contexts, assertionsInSubjectContext, language, hasLanguage, includeInferred, field);
        this.elementDescriptor = elementDescriptor;
    }

    @Override
    public Descriptor getAttributeDescriptor(FieldSpecification<?, ?> attribute) {
        Objects.requireNonNull(attribute);
        if (getField().equals(attribute.getJavaField())) {
            return this;
        }
        return elementDescriptor.getAttributeDescriptor(attribute);
    }

    @Override
    public Set<URI> getAttributeContexts(FieldSpecification<?, ?> attribute) {
        Objects.requireNonNull(attribute);
        if (getField().equals(attribute.getJavaField())) {
            return getContexts();
        }
        return elementDescriptor.getAttributeContexts(attribute);
    }

    @Override
    public ObjectPropertyCollectionDescriptor addAttributeDescriptor(FieldSpecification<?, ?> attribute,
                                                                     Descriptor descriptor) {
        elementDescriptor.addAttributeDescriptor(attribute, descriptor);
        return this;
    }

    @Override
    public ObjectPropertyCollectionDescriptor addAttributeContext(FieldSpecification<?, ?> attribute, URI context) {
        elementDescriptor.addAttributeContext(attribute, context);
        return this;
    }

    @Override
    public Descriptor addContext(URI context) {
        if (context == null) {
            elementDescriptor.contexts.clear();
            contexts.clear();
        } else {
            elementDescriptor.addContext(context);
            contexts.add(context);
        }
        return this;
    }

    @Override
    public boolean overridesAssertionContext() {
        return false;
    }

    @Override
    public EntityDescriptor unwrap() {
        return elementDescriptor;
    }

    @Override
    public ObjectPropertyCollectionDescriptor copy() {
        return new ObjectPropertyCollectionDescriptor(contexts, assertionsInSubjectContext, getLanguage(), hasLanguage(), includeInferred(), getField(), elementDescriptor.copy());
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
        if (this == obj) {return true;}
        if (!super.equals(obj)) {return false;}
        if (getClass() != obj.getClass()) {return false;}
        ObjectPropertyCollectionDescriptor other = (ObjectPropertyCollectionDescriptor) obj;
        if (elementDescriptor == null) {
            return other.elementDescriptor == null;
        } else {return elementDescriptor.equals(other.elementDescriptor);}
    }
}

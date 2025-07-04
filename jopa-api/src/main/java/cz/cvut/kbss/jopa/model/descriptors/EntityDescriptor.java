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

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.Attribute.PersistentAttributeType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.QueryAttribute;
import cz.cvut.kbss.jopa.model.metamodel.Type;
import cz.cvut.kbss.ontodriver.util.IdentifierUtils;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;

/**
 * Describes an entity.
 * <p>
 * Each attribute has a descriptor associated with it.
 */
public class EntityDescriptor extends AbstractDescriptor {

    private final Map<Field, Descriptor> fieldDescriptors;

    public EntityDescriptor() {
        this.fieldDescriptors = new HashMap<>();
    }

    /**
     * Allows configuring where object property assertions should be stored
     *
     * @param assertionsInSubjectContext Whether object property assertions are stored in the subject's. Defaults to
     *                                   {@code true}. If {@code false}, object property assertions are stored in the
     *                                   object's context
     * @see #areAssertionsInSubjectContext()
     */
    public EntityDescriptor(boolean assertionsInSubjectContext) {
        super(assertionsInSubjectContext);
        this.fieldDescriptors = new HashMap<>();
    }

    public EntityDescriptor(URI context) {
        super(context);
        this.fieldDescriptors = new HashMap<>();
    }

    public EntityDescriptor(Set<URI> contexts) {
        this();
        this.contexts.addAll(Objects.requireNonNull(contexts));
    }

    public EntityDescriptor(URI context, boolean assertionsInSubjectContext) {
        super(context, assertionsInSubjectContext);
        this.fieldDescriptors = new HashMap<>();
    }

    protected EntityDescriptor(Set<URI> contexts, boolean assertionsInSubjectContext, String language,
                               boolean hasLanguage,
                               boolean includeInferred, Map<Field, Descriptor> fieldDescriptors) {
        super(contexts, assertionsInSubjectContext, language, hasLanguage, includeInferred);
        this.fieldDescriptors = new HashMap<>(fieldDescriptors.size());
        fieldDescriptors.forEach((f, d) -> this.fieldDescriptors.put(f, d.copy()));
    }

    @Override
    public EntityDescriptor addAttributeDescriptor(FieldSpecification<?, ?> attribute, Descriptor descriptor) {
        Objects.requireNonNull(attribute);
        Objects.requireNonNull(descriptor);

        if (isPluralReference(attribute) && descriptor instanceof EntityDescriptor entityDescriptor) {
            fieldDescriptors.put(attribute.getJavaField(), new ObjectPropertyCollectionDescriptor(attribute, entityDescriptor));
        } else {
            fieldDescriptors.put(attribute.getJavaField(), descriptor);
        }
        return this;
    }

    private static boolean isPluralReference(FieldSpecification<?, ?> attribute) {
        return attribute instanceof PluralAttribute<?, ?, ?> pluralAtt &&
                pluralAtt.getElementType().getPersistenceType() == Type.PersistenceType.ENTITY;
    }

    @Override
    public EntityDescriptor addAttributeContext(FieldSpecification<?, ?> attribute, URI context) {
        Objects.requireNonNull(attribute);

        fieldDescriptors.putIfAbsent(attribute.getJavaField(),
                createDescriptor(attribute, context != null ? Collections.singleton(context) : Collections.emptySet()));
        fieldDescriptors.get(attribute.getJavaField()).addContext(context);
        return this;
    }

    @Override
    public EntityDescriptor setLanguage(String languageTag) {
        super.setLanguage(languageTag);
        fieldDescriptors.values().forEach(d -> d.setLanguage(languageTag));
        return this;
    }

    @Override
    public EntityDescriptor setAttributeLanguage(FieldSpecification<?, ?> attribute, String languageTag) {
        Objects.requireNonNull(attribute);

        fieldDescriptors.putIfAbsent(attribute.getJavaField(), createDescriptor(attribute, getContexts()));
        fieldDescriptors.get(attribute.getJavaField()).setLanguage(languageTag);
        return this;
    }

    @Override
    public Descriptor getAttributeDescriptor(FieldSpecification<?, ?> attribute) {
        Objects.requireNonNull(attribute);
        Descriptor d = fieldDescriptors.get(attribute.getJavaField());
        if (d == null) {
            d = createDescriptor(attribute, getContexts());
            if (hasLanguage()) {
                d.setLanguage(getLanguage());
            }
        }
        return d;
    }

    @Override
    public Set<URI> getAttributeContexts(FieldSpecification<?, ?> attribute) {
        Objects.requireNonNull(attribute);
        final Descriptor attDescriptor = getAttributeDescriptor(attribute);
        return attDescriptor.overridesAssertionContext() || !assertionsInSubjectContext ?
                attDescriptor.getContexts() : getContexts();
    }

    @Override
    public Collection<Descriptor> getAttributeDescriptors() {
        return Collections.unmodifiableCollection(fieldDescriptors.values());
    }

    private AbstractDescriptor createDescriptor(FieldSpecification<?, ?> att, Set<URI> contexts) {
        final AbstractDescriptor result;
        if (att instanceof Attribute<?, ?> attSpec) {
            if (attSpec.getPersistentAttributeType() == PersistentAttributeType.OBJECT) {
                if (attSpec.isCollection()) {
                    result = new ObjectPropertyCollectionDescriptor(contexts, att);
                } else {
                    result = IdentifierUtils.isResourceIdentifierType(attSpec.getJavaType()) ? new FieldDescriptor(contexts, att) : new EntityDescriptor(contexts);
                }
            } else {
                result = new FieldDescriptor(contexts, att);
            }
        } else if (att instanceof QueryAttribute) {
            result = new EntityDescriptor(contexts);
        } else {
            result = new FieldDescriptor(contexts, att);
        }
        result.setIncludeInferred(includeInferred());
        return result;
    }

    @Override
    public EntityDescriptor copy() {
        return new EntityDescriptor(contexts, assertionsInSubjectContext, getLanguage(), hasLanguage(), includeInferred(), fieldDescriptors);
    }

    @Override
    public boolean equals(Object o) {
        return equals(o, new HashMap<>());
    }

    @Override
    protected boolean equals(Object other, Map<VisitedPair, Boolean> visited) {
        if (this == other) {
            return true;
        }
        if (!(other instanceof EntityDescriptor that)) {
            return false;
        }
        final VisitedPair pair = new VisitedPair(this, that);
        if (visited.containsKey(pair)) {
            return visited.get(pair);
        }
        visited.put(pair, Boolean.TRUE);
        boolean result = super.equalsImpl(other) && fieldDescriptors.size() == that.fieldDescriptors.size();

        for (Entry<Field, Descriptor> e : fieldDescriptors.entrySet()) {
            if (e.getValue() == null) {
                if (that.fieldDescriptors.containsKey(e.getKey()) && that.fieldDescriptors.get(e.getKey()) != null) {
                    result = false;
                    break;
                }
            } else {
                if (!(e.getValue() instanceof AbstractDescriptor)) {
                    if (!e.getValue().equals(that.fieldDescriptors.get(e.getKey()))) {
                        result = false;
                        break;
                    }
                } else if (!((AbstractDescriptor) e.getValue()).equals(that.fieldDescriptors.get(e.getKey()), visited)) {
                    result = false;
                    break;
                }
            }
        }
        visited.put(pair, result);
        return result;
    }

    @Override
    public int hashCode() {
        return this.hashCode(new IdentityHashMap<>());
    }

    @Override
    protected int hashCode(Map<Object, Boolean> visited) {
        if (visited.containsKey(this)) {
            return 0;
        }
        visited.put(this, Boolean.TRUE);
        return 31 * super.hashCodeImpl() + fieldDescriptors.entrySet().stream()
                                                           .map(e -> e.getKey().hashCode() ^
                                                                   (e.getValue() == this ? 0 :
                                                                           e.getValue().hashCode()))
                                                           .reduce(0, Integer::sum);
    }
}

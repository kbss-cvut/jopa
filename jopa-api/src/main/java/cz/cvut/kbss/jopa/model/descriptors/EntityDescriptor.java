/**
 * Copyright (C) 2020 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.Attribute.PersistentAttributeType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.utils.ErrorUtils;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.*;
import java.util.Map.Entry;

/**
 * Describes an entity.
 * <p>
 * Each attribute has a descriptor associated with it.
 */
public class EntityDescriptor extends Descriptor {

    private final Map<Field, Descriptor> fieldDescriptors;

    public EntityDescriptor() {
        this.fieldDescriptors = new HashMap<>();
    }

    /**
     * Allows to configure where object property assertions should be stored
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

    public EntityDescriptor(URI context, boolean assertionsInSubjectContext) {
        super(context, assertionsInSubjectContext);
        this.fieldDescriptors = new HashMap<>();
    }

    @Override
    public EntityDescriptor addAttributeDescriptor(Field attribute, Descriptor descriptor) {
        Objects.requireNonNull(attribute, ErrorUtils.getNPXMessageSupplier("attribute"));
        Objects.requireNonNull(descriptor, ErrorUtils.getNPXMessageSupplier("descriptor"));

        fieldDescriptors.put(attribute, descriptor);
        return this;
    }

    @Override
    public EntityDescriptor addAttributeContext(Field attribute, URI context) {
        Objects.requireNonNull(attribute, ErrorUtils.getNPXMessageSupplier("attribute"));

        fieldDescriptors.put(attribute, new FieldDescriptor(context, attribute));
        return this;
    }

    @Override
    public EntityDescriptor setLanguage(String languageTag) {
        super.setLanguage(languageTag);
        fieldDescriptors.values().forEach(d -> d.setLanguage(languageTag));
        return this;
    }

    @Override
    public EntityDescriptor setAttributeLanguage(Field attribute, String languageTag) {
        Objects.requireNonNull(attribute);

        fieldDescriptors.putIfAbsent(attribute, new FieldDescriptor(null, attribute));
        fieldDescriptors.get(attribute).setLanguage(languageTag);
        return this;
    }

    @Override
    public Descriptor getAttributeDescriptor(FieldSpecification<?, ?> attribute) {
        Objects.requireNonNull(attribute);
        Descriptor d = getFieldDescriptor(attribute.getJavaField());
        if (d == null) {
            d = createDescriptor(attribute, context);
            if (hasLanguage()) {
                d.setLanguage(getLanguage());
            }
        }
        return d;
    }

    @Override
    public URI getAttributeContext(FieldSpecification<?, ?> attribute) {
        Objects.requireNonNull(attribute);
        final Descriptor attDescriptor = getAttributeDescriptor(attribute);
        return attDescriptor.overridesAssertionsInSubjectContext() || !assertionsInSubjectContext ?
               attDescriptor.getContext() : getContext();
    }

    @Override
    public Collection<Descriptor> getAttributeDescriptors() {
        return Collections.unmodifiableCollection(fieldDescriptors.values());
    }

    private Descriptor getFieldDescriptor(Field field) {
        for (Entry<Field, Descriptor> e : fieldDescriptors.entrySet()) {
            if (e.getKey().equals(field)) {
                return e.getValue();
            }
        }
        return null;
    }

    private static Descriptor createDescriptor(FieldSpecification<?, ?> att, URI context) {
        if (att instanceof Attribute) {
            final Attribute<?, ?> attSpec = (Attribute<?, ?>) att;
            if (attSpec.getPersistentAttributeType() == PersistentAttributeType.OBJECT) {
                if (attSpec.isCollection()) {
                    return new ObjectPropertyCollectionDescriptor(context, att.getJavaField());
                } else {
                    return new EntityDescriptor(context);
                }
            }
        }
        return new FieldDescriptor(context, att.getJavaField());
    }

    @Override
    protected Set<URI> getContextsInternal(Set<URI> contexts, Set<Descriptor> visited) {
        if (visited.contains(this)) {
            return contexts;
        }
        if (context == null) {
            return null;
        }
        contexts.add(context);
        visited.add(this);
        for (Descriptor fd : fieldDescriptors.values()) {
            contexts = fd.getContextsInternal(contexts, visited);
            if (contexts == null) {
                return null;
            }
        }
        return contexts;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof EntityDescriptor)) {
            return false;
        }
        if (!super.equals(o)) {
            return false;
        }

        EntityDescriptor that = (EntityDescriptor) o;

        if (fieldDescriptors.size() != that.fieldDescriptors.size()) {
            return false;
        }
        for (Entry<Field, Descriptor> e : fieldDescriptors.entrySet()) {
            if (e.getValue() == null) {
                if (that.fieldDescriptors.containsKey(e.getKey()) && that.fieldDescriptors.get(e.getKey()) != null) {
                    return false;
                }
            } else {
                if (e.getValue() == this && that.fieldDescriptors.get(e.getKey()) == that) {
                    continue;
                }
                if (!e.getValue().equals(that.fieldDescriptors.get(e.getKey()))) {
                    return false;
                }
            }
        }
        return true;
    }

    @Override
    public int hashCode() {
        int result = super.hashCode();
        result = 31 * result + (fieldDescriptors != null ? fieldDescriptors.entrySet().stream()
                                                                           .map(e -> e.getKey().hashCode() ^
                                                                                   (e.getValue() == this ? 0 :
                                                                                    e.getValue().hashCode())).reduce(0,
                        Integer::sum) : 0);
        return result;
    }
}

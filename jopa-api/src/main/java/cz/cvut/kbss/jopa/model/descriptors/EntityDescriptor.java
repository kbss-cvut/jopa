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
package cz.cvut.kbss.jopa.model.descriptors;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.Attribute.PersistentAttributeType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
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

    public EntityDescriptor(URI context) {
        super(context);
        this.fieldDescriptors = new HashMap<>();
    }

    @Override
    public void addAttributeDescriptor(Field attribute, Descriptor descriptor) {
        Objects.requireNonNull(attribute, ErrorUtils.getNPXMessageSupplier("attribute"));
        Objects.requireNonNull(descriptor, ErrorUtils.getNPXMessageSupplier("descriptor"));

        fieldDescriptors.put(attribute, descriptor);
    }

    @Override
    public void addAttributeContext(Field attribute, URI context) {
        Objects.requireNonNull(attribute, ErrorUtils.getNPXMessageSupplier("attribute"));

        fieldDescriptors.put(attribute, new FieldDescriptor(context, attribute));
    }

    @Override
    public void setLanguage(String languageTag) {
        super.setLanguage(languageTag);
        fieldDescriptors.values().forEach(d -> d.setLanguage(languageTag));
    }

    @Override
    public void setAttributeLanguage(Field attribute, String languageTag) {
        Objects.requireNonNull(attribute);

        fieldDescriptors.putIfAbsent(attribute, new FieldDescriptor(null, attribute));
        fieldDescriptors.get(attribute).setLanguage(languageTag);
    }

    @Override
    public Descriptor getAttributeDescriptor(FieldSpecification<?, ?> attribute) {
        Descriptor d = getFieldDescriptor(attribute.getJavaField());
        if (d == null) {
            d = createDescriptor(attribute, context);
            d.setLanguage(language);
        }
        return d;
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
        if ((att instanceof TypesSpecification) || (att instanceof PropertiesSpecification)) {
            return new FieldDescriptor(context, att.getJavaField());
        }
        final Attribute<?, ?> attSpec = (Attribute<?, ?>) att;
        if (attSpec.getPersistentAttributeType() == PersistentAttributeType.OBJECT) {
            if (attSpec.isCollection()) {
                return new ObjectPropertyCollectionDescriptor(context, att.getJavaField());
            } else {
                return new EntityDescriptor(context);
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
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((fieldDescriptors == null) ? 0 : fieldDescriptors.hashCode());
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
        EntityDescriptor other = (EntityDescriptor) obj;
        if (fieldDescriptors == null) {
            if (other.fieldDescriptors != null)
                return false;
        } else if (!fieldDescriptors.equals(other.fieldDescriptors))
            return false;
        return true;
    }

}

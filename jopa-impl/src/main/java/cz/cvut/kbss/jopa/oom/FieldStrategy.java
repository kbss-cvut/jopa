/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.*;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.ontodriver.exception.NotYetImplementedException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;

import java.net.URI;

abstract class FieldStrategy<T extends FieldSpecification<? super X, ?>, X> {

    final EntityType<X> et;
    final T attribute;
    final Descriptor attributeDescriptor;
    final EntityMappingHelper mapper;
    ReferenceSavingResolver referenceSavingResolver;

    FieldStrategy(EntityType<X> et, T att, Descriptor attributeDescriptor, EntityMappingHelper mapper) {
        this.et = et;
        this.attribute = att;
        this.attributeDescriptor = attributeDescriptor;
        this.mapper = mapper;
    }

    static <X> FieldStrategy<? extends FieldSpecification<? super X, ?>, X> createFieldStrategy(
            EntityType<X> et, FieldSpecification<? super X, ?> att,
            Descriptor fieldDescriptor, EntityMappingHelper mapper) {
        if (att.equals(et.getIdentifier())) {
            return new IdentifierFieldStrategy<>(et, (Identifier<? super X, ?>) att, fieldDescriptor, mapper);
        }
        if (att instanceof TypesSpecification) {
            return new TypesFieldStrategy<>(et, (TypesSpecification<? super X, ?>) att, fieldDescriptor, mapper);
        } else if (att instanceof PropertiesSpecification) {
            return new PropertiesFieldStrategy<>(et, (PropertiesSpecification<? super X, ?, ?, ?>) att, fieldDescriptor,
                    mapper);
        }
        final AbstractAttribute<? super X, ?> attribute = (AbstractAttribute<? super X, ?>) att;
        if (attribute.isCollection()) {
            switch (attribute.getPersistentAttributeType()) {
                case ANNOTATION:
                    return new PluralAnnotationPropertyStrategy<>(et,
                            (AbstractPluralAttribute<? super X, ?, ?>) attribute, fieldDescriptor, mapper);
                case DATA:
                    return new PluralDataPropertyStrategy<>(et, (AbstractPluralAttribute<? super X, ?, ?>) attribute,
                            fieldDescriptor, mapper);
                case OBJECT:
                    return createPluralObjectPropertyStrategy(et, (AbstractPluralAttribute<? super X, ?, ?>) attribute,
                            fieldDescriptor, mapper);
                default:
                    break;
            }
        } else {
            switch (attribute.getPersistentAttributeType()) {
                case ANNOTATION:
                    return new SingularAnnotationPropertyStrategy<>(et, attribute, fieldDescriptor, mapper);
                case DATA:
                    return new SingularDataPropertyStrategy<>(et, attribute, fieldDescriptor, mapper);
                case OBJECT:
                    return new SingularObjectPropertyStrategy<>(et, attribute, fieldDescriptor, mapper);
                default:
                    break;
            }
        }
        // Shouldn't happen
        throw new IllegalArgumentException();
    }

    private static <Y> FieldStrategy<? extends FieldSpecification<? super Y, ?>, Y> createPluralObjectPropertyStrategy(
            EntityType<Y> et, AbstractPluralAttribute<? super Y, ?, ?> attribute,
            Descriptor descriptor, EntityMappingHelper mapper) {
        switch (attribute.getCollectionType()) {
            case LIST:
                return createOwlListPropertyStrategy(et, (ListAttributeImpl<? super Y, ?>) attribute, descriptor,
                        mapper);
            case COLLECTION:
            case SET:
                return new SimpleSetPropertyStrategy<>(et, attribute, descriptor, mapper);
            default:
                throw new NotYetImplementedException(
                        "Unsupported plural attribute collection type " + attribute.getCollectionType());
        }
    }

    private static <Y> FieldStrategy<? extends FieldSpecification<? super Y, ?>, Y> createOwlListPropertyStrategy(
            EntityType<Y> et, ListAttributeImpl<? super Y, ?> attribute, Descriptor descriptor,
            EntityMappingHelper mapper) {
        switch (attribute.getSequenceType()) {
            case referenced:
                return new ReferencedListPropertyStrategy<>(et, attribute, descriptor, mapper);
            case simple:
                return new SimpleListPropertyStrategy<>(et, attribute, descriptor, mapper);
            default:
                throw new NotYetImplementedException(
                        "Unsupported list attribute sequence type " + attribute.getSequenceType());
        }
    }

    void setReferenceSavingResolver(ReferenceSavingResolver referenceSavingResolver) {
        this.referenceSavingResolver = referenceSavingResolver;
    }

    /**
     * Sets the specified value on the specified instance, the field is taken from the attribute represented by this
     * strategy. </p>
     * <p>
     * Note that this method assumes the value and the field are of compatible types, no check is done here.
     */
    void setValueOnInstance(Object instance, Object value) {
        EntityPropertiesUtils.setFieldValue(attribute.getJavaField(), instance, value);
    }

    /**
     * Extracts the attribute value from the specified instance. </p>
     *
     * @return Attribute value, possibly {@code null}
     */
    Object extractFieldValueFromInstance(Object instance) {
        return EntityPropertiesUtils.getAttributeValue(attribute, instance);
    }

    <E> URI resolveValueIdentifier(E instance, EntityType<E> valEt) {
        URI id = EntityPropertiesUtils.getIdentifier(instance, valEt);
        if (id == null) {
            id = mapper.generateIdentifier(valEt);
            EntityPropertiesUtils.setIdentifier(id, instance, valEt);
        }
        return id;
    }

    URI getAttributeContext() {
        return attributeDescriptor.getContext();
    }

    /**
     * Gets the language tag that should be used when mapping this field to an assertion.
     *
     * @return Language tag, possibly {@code null}
     */
    String getLanguage() {
        return attributeDescriptor.hasLanguage() ? attributeDescriptor.getLanguage() :
                mapper.getConfiguration().get(JOPAPersistenceProperties.LANG);
    }

    /**
     * Adds value from the specified axioms to this strategy. </p>
     * <p>
     * The value(s) is/are then set on entity field using {@link #buildInstanceFieldValue(Object)}.
     *
     * @param ax Axiom to extract value from
     */
    abstract void addValueFromAxiom(Axiom<?> ax);

    /**
     * Sets instance field from values gathered in this strategy.
     *
     * @param instance The instance to receive the field value
     * @throws IllegalArgumentException Access error
     * @throws IllegalAccessException   Access error
     */
    abstract void buildInstanceFieldValue(Object instance) throws IllegalAccessException;

    /**
     * Extracts values of field represented by this strategy from the specified instance.
     *
     * @param instance     The instance to extract values from
     * @param valueBuilder Builder into which the attribute value(s) are extracted
     * @throws IllegalArgumentException Access error
     * @throws IllegalAccessException   Access error
     */
    abstract void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder)
            throws IllegalAccessException;

    /**
     * Creates property assertion appropriate for the attribute represented by this strategy.
     *
     * @return Property assertion
     */
    abstract Assertion createAssertion();
}

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
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.*;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.ontodriver.model.*;

import java.net.URI;
import java.util.Collection;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @param <T> The attribute specification type, e.g. {@link SingularAttribute}, {@link ListAttribute}
 * @param <X> Entity class
 */
abstract class FieldStrategy<T extends FieldSpecification<? super X, ?>, X> {

    final EntityType<X> et;
    final T attribute;
    final Descriptor entityDescriptor;
    final EntityMappingHelper mapper;
    ReferenceSavingResolver referenceSavingResolver;

    FieldStrategy(EntityType<X> et, T att, Descriptor entityDescriptor, EntityMappingHelper mapper) {
        this.et = et;
        this.attribute = att;
        this.entityDescriptor = entityDescriptor;
        this.mapper = mapper;
    }

    static <X> FieldStrategy<? extends FieldSpecification<? super X, ?>, X> createFieldStrategy(EntityType<X> et,
                                                                                                FieldSpecification<? super X, ?> att,
                                                                                                Descriptor entityDescriptor,
                                                                                                EntityMappingHelper mapper) {
        if (att.equals(et.getIdentifier())) {
            return new IdentifierFieldStrategy<>(et, (Identifier<? super X, ?>) att, entityDescriptor, mapper);
        }
        if (att instanceof TypesSpecification) {
            return new TypesFieldStrategy<>(et, (TypesSpecification<? super X, ?>) att, entityDescriptor, mapper);
        } else if (att instanceof PropertiesSpecification) {
            return new PropertiesFieldStrategy<>(et, (PropertiesSpecification<? super X, ?, ?, ?>) att,
                    entityDescriptor, mapper);
        }
        final AbstractAttribute<? super X, ?> attribute = (AbstractAttribute<? super X, ?>) att;
        if (attribute.isCollection()) {
            switch (attribute.getPersistentAttributeType()) {
                case ANNOTATION:
                    return createPluralAnnotationPropertyStrategy(et,
                            (AbstractPluralAttribute<? super X, ?, ?>) attribute,
                            entityDescriptor, mapper);
                case DATA:
                    return createPluralDataPropertyStrategy(et, (AbstractPluralAttribute<? super X, ?, ?>) attribute,
                            entityDescriptor, mapper);
                case OBJECT:
                    return createPluralObjectPropertyStrategy(et, (AbstractPluralAttribute<? super X, ?, ?>) attribute,
                            entityDescriptor, mapper);
                default:
                    break;
            }
        } else {
            switch (attribute.getPersistentAttributeType()) {
                case ANNOTATION:
                    return createSingularAnnotationPropertyStrategy(et, attribute, entityDescriptor, mapper);
                case DATA:
                    return createSingularDataPropertyStrategy(et, attribute, entityDescriptor, mapper);
                case OBJECT:
                    return new SingularObjectPropertyStrategy<>(et, attribute, entityDescriptor, mapper);
                default:
                    break;
            }
        }
        // Shouldn't happen
        throw new IllegalArgumentException();
    }

    private static <Y> FieldStrategy<? extends FieldSpecification<? super Y, ?>, Y> createPluralAnnotationPropertyStrategy(
            EntityType<Y> et, AbstractPluralAttribute<? super Y, ?, ?> attribute, Descriptor descriptor,
            EntityMappingHelper mapper) {
        if (MultilingualString.class.equals(attribute.getElementType().getJavaType())) {
            return new PluralMultilingualStringFieldStrategy<>(et,
                    (AbstractPluralAttribute<? super Y, ?, MultilingualString>) attribute,
                    descriptor, mapper);
        } else {
            return new PluralAnnotationPropertyStrategy<>(et, attribute, descriptor, mapper);
        }
    }

    private static <Y> FieldStrategy<? extends FieldSpecification<? super Y, ?>, Y> createPluralDataPropertyStrategy(
            EntityType<Y> et, AbstractPluralAttribute<? super Y, ?, ?> attribute, Descriptor descriptor,
            EntityMappingHelper mapper) {
        if (MultilingualString.class.equals(attribute.getElementType().getJavaType())) {
            return new PluralMultilingualStringFieldStrategy<>(et,
                    (AbstractPluralAttribute<? super Y, ?, MultilingualString>) attribute,
                    descriptor, mapper);
        } else {
            return new PluralDataPropertyStrategy<>(et, attribute, descriptor, mapper);
        }
    }

    private static <Y> FieldStrategy<? extends FieldSpecification<? super Y, ?>, Y> createPluralObjectPropertyStrategy(
            EntityType<Y> et, AbstractPluralAttribute<? super Y, ?, ?> attribute, Descriptor descriptor,
            EntityMappingHelper mapper) {
        switch (attribute.getCollectionType()) {
            case LIST:
                return createOwlListPropertyStrategy(et, (ListAttributeImpl<? super Y, ?>) attribute, descriptor,
                        mapper);
            case COLLECTION:
            case SET:
                return new SimpleSetPropertyStrategy<>(et, attribute, descriptor, mapper);
            default:
                throw new UnsupportedOperationException(
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
                throw new UnsupportedOperationException(
                        "Unsupported list attribute sequence type " + attribute.getSequenceType());
        }
    }

    private static <X> FieldStrategy<? extends FieldSpecification<? super X, ?>, X> createSingularDataPropertyStrategy(
            EntityType<X> et, AbstractAttribute<? super X, ?> attribute, Descriptor descriptor,
            EntityMappingHelper mapper) {
        if (MultilingualString.class.equals(attribute.getJavaType())) {
            return new SingularMultilingualStringFieldStrategy<>(et,
                    (AbstractAttribute<? super X, MultilingualString>) attribute,
                    descriptor, mapper);
        } else {
            return new SingularDataPropertyStrategy<>(et, attribute, descriptor, mapper);
        }
    }

    private static <X> FieldStrategy<? extends FieldSpecification<? super X, ?>, X> createSingularAnnotationPropertyStrategy(
            EntityType<X> et, AbstractAttribute<? super X, ?> attribute, Descriptor descriptor,
            EntityMappingHelper mapper) {
        if (MultilingualString.class.equals(attribute.getJavaType())) {
            return new SingularMultilingualStringFieldStrategy<>(et,
                    (AbstractAttribute<? super X, MultilingualString>) attribute,
                    descriptor, mapper);
        } else {
            return new SingularAnnotationPropertyStrategy<>(et, attribute, descriptor, mapper);
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

    /**
     * Gets the context to which this attribute assertions are written.
     * <p>
     * Note that this may not (and in case of object properties usually won't) be the context containing the target
     * object. Rather, it will be the context of the owner entity itself (depending on whether assertions are stored in
     * subject context or not).
     *
     * @return Attribute assertion context
     * @see Descriptor
     */
    URI getAttributeWriteContext() {
        return entityDescriptor.getSingleAttributeContext(attribute).orElse(null);
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
     */
    abstract void buildInstanceFieldValue(Object instance);

    /**
     * Extracts values of field represented by this strategy from the specified instance and adds them to the specified
     * value gatherer.
     *
     * @param instance     The instance to extract values from
     * @param valueBuilder Builder into which the attribute value(s) are extracted
     * @throws IllegalArgumentException Access error
     */
    abstract void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder);

    /**
     * Extracts values of field represented by this strategy from the specified instance and returns axioms representing
     * them.
     *
     * @param instance The instance to extract values from
     * @return Set of axioms
     * @throws IllegalArgumentException Access error
     */
    abstract Set<Axiom<?>> buildAxiomsFromInstance(X instance);

    /**
     * Creates property assertion appropriate for the attribute represented by this strategy.
     *
     * @return Property assertion
     */
    abstract Assertion createAssertion();

    /**
     * Transforms the specified attribute value to an axiom {@link Value}.
     *
     * @param value Value to transform
     * @return Axiom value
     */
    abstract Collection<Value<?>> toAxiomValue(Object value);

    /**
     * Returns only values that are not inferred in the repository for the specified subject.
     * <p>
     * This method goes through the specified values and if the property represented by this strategy can contain
     * inferred values (i.e., {@link FieldSpecification#isInferred()} returns true), it returns only those values that
     * are NOT inferred in the repository.
     * <p>
     * The reasoning of this method is that the current implementation of a property values update is done by removing
     * all its values as asserting new ones extracted from the entity. However, if an attribute value is a mix of
     * inferred and asserted values, this will basically assert values that are already inferred, which is not correct.
     * This method thus removes inferred values from the values passed to the OntoDriver for saving. Note that there is
     * a minor caveat in that one is not able to intentionally assert already inferred values this way, but it is
     * considered not important to work resolve at the moment.
     *
     * @param subject Subject whose property values are examined
     * @param values  Values to filter
     * @return Asserted values
     */
    protected Set<Value<?>> filterOutInferredValues(NamedResource subject, Set<Value<?>> values) {
        if (!attribute.isInferred()) {
            return values;
        }
        final Assertion assertion = createAssertion();
        final URI ctx = getAttributeWriteContext();
        return values.stream().filter(v -> !mapper.isInferred(new AxiomImpl<>(subject, assertion, v), ctx))
                     .collect(Collectors.toSet());
    }

    @Override
    public String toString() {
        return getClass().getSimpleName() + "{" +
                "attribute=" + attribute +
                '}';
    }
}

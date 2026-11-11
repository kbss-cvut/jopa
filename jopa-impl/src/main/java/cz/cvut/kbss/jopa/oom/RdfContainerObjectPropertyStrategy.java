/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.RDFContainerAttribute;
import cz.cvut.kbss.jopa.model.metamodel.RdfContainerAttributeImpl;
import cz.cvut.kbss.jopa.oom.util.ObjectGraphInfo;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.ontodriver.descriptor.ContainerDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ContainerValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;
import java.util.Collection;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Consumer;

class RdfContainerObjectPropertyStrategy<X> extends PluralObjectPropertyStrategy<RdfContainerAttributeImpl<? super X, ?, ?>, X> {

    RdfContainerObjectPropertyStrategy(EntityType<X> et, RdfContainerAttributeImpl<? super X, ?, ?> att,
                                       ObjectGraphInfo objectGraphInfo, EntityMappingHelper mapper) {
        super(et, att, objectGraphInfo, mapper);
    }

    @Override
    void addAxiomValue(Axiom<?> ax) {
        new RdfContainerLoader<>(attribute, mapper, this).loadFromAxiom(ax, super::addAxiomValue);
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) {
        final RdfContainerSaver<X> saver = new RdfContainerSaver<>(this, attribute);
        saver.save(instance, valueBuilder, getValueMapper());
    }

    private <T> BiFunction<Object, ContainerValueDescriptor<Object>, Optional<Object>> getValueMapper() {
        final Class<?> elemType = attribute.getBindableJavaType();
        if (IdentifierTransformer.isValidIdentifierType(elemType)) {
            return (item, _) -> Optional.of(NamedResource.create(IdentifierTransformer.valueAsUri(item)));
        } else if (elemType.isEnum()) {
            assert attribute.getConverter() != null;
            return (item, _) -> Optional.of(attribute.getConverter().convertToAxiomValue(item));
        } else {
            final EntityType<T> et = (EntityType<T>) mapper.getEntityType(elemType);
            return (item, valueDescriptor) -> {
                if (referenceSavingResolver.shouldSaveReferenceToItem(item, getAttributeValueContexts())) {
                    final URI valId = EntityPropertiesUtils.getIdentifier(item, et);
                    assert valId != null;
                    return Optional.of(NamedResource.create(valId));
                } else {
                    referenceSavingResolver.registerPendingReference(valueDescriptor.getOwner(), createAssertion(), item, getAttributeWriteContext());
                    return Optional.empty();
                }
            };
        }
    }

    @Override
    Set<Axiom<?>> buildAxiomsFromInstance(X instance) {
        throw new UnsupportedOperationException("Method not supported for RDF containers.");
    }

    static class RdfContainerLoader<X> {
        private final RDFContainerAttribute<X, ?, ?> attribute;
        private final EntityMappingHelper mapper;
        private final FieldStrategy<?, ?> owner;

        RdfContainerLoader(RDFContainerAttribute<X, ?, ?> attribute, EntityMappingHelper mapper,
                           FieldStrategy<?, ?> owner) {
            this.attribute = attribute;
            this.mapper = mapper;
            this.owner = owner;
        }

        void loadFromAxiom(Axiom<?> axiom, Consumer<Axiom<?>> valueConsumer) {
            final NamedResource subject = axiom.getSubject();
            final Assertion assertion = owner.createAssertion();
            final ContainerDescriptor desc = switch (attribute.getContainerType()) {
                case SEQ -> ContainerDescriptor.seqDescriptor(subject, assertion, owner.getAttributeWriteContext());
                case ALT -> ContainerDescriptor.altDescriptor(subject, assertion, owner.getAttributeWriteContext());
                case BAG -> ContainerDescriptor.bagDescriptor(subject, assertion, owner.getAttributeWriteContext());
            };
            mapper.loadRdfContainer(desc).forEach(valueConsumer);
        }
    }

    static class RdfContainerSaver<X> {
        private final FieldStrategy<?, ?> fieldStrategy;
        private final RDFContainerAttribute<?, ?, ?> attribute;

        RdfContainerSaver(FieldStrategy<?, ?> fieldStrategy, RDFContainerAttribute<?, ?, ?> attribute) {
            this.fieldStrategy = fieldStrategy;
            this.attribute = attribute;
        }

        <T> ContainerValueDescriptor<T> createContainerValueDescriptor(X owner) {
            final NamedResource subject = NamedResource.create(EntityPropertiesUtils.getIdentifier(owner, fieldStrategy.et));
            return switch (attribute.getContainerType()) {
                case SEQ ->
                        ContainerValueDescriptor.seqValueDescriptor(subject, fieldStrategy.createAssertion(), fieldStrategy.getAttributeWriteContext());
                case ALT ->
                        ContainerValueDescriptor.altValueDescriptor(subject, fieldStrategy.createAssertion(), fieldStrategy.getAttributeWriteContext());
                case BAG ->
                        ContainerValueDescriptor.bagValueDescriptor(subject, fieldStrategy.createAssertion(), fieldStrategy.getAttributeWriteContext());
            };
        }

        void save(X owner, AxiomValueGatherer valueBuilder,
                  BiFunction<Object, ContainerValueDescriptor<Object>, Optional<Object>> valueMapper) {
            final Object value = fieldStrategy.extractFieldValueFromInstance(owner);
            assert value instanceof Collection || value == null;
            final Collection<?> valueCollection = (Collection<?>) value;
            final ContainerValueDescriptor<Object> valueDescriptor = createContainerValueDescriptor(owner);
            if (valueCollection != null) {
                valueCollection.stream()
                               .filter(Objects::nonNull)
                               .map(item -> valueMapper.apply(item, valueDescriptor))
                               .filter(Optional::isPresent)
                               .forEach(opt -> valueDescriptor.addValue(opt.get()));
            }
            valueBuilder.addContainerValues(valueDescriptor);
        }
    }
}

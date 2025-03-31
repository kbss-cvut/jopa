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
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.annotations.RDFContainerType;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.RdfContainerAttributeImpl;
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
import java.util.Set;

class RdfContainerObjectPropertyStrategy<X> extends PluralObjectPropertyStrategy<RdfContainerAttributeImpl<? super X, ?, ?>, X> {

    private final RDFContainerType containerType;

    RdfContainerObjectPropertyStrategy(EntityType<X> et, RdfContainerAttributeImpl<? super X, ?, ?> att,
                                       Descriptor descriptor, EntityMappingHelper mapper) {
        super(et, att, descriptor, mapper);
        this.containerType = att.getContainerType();
    }

    @Override
    void addAxiomValue(Axiom<?> ax) {
        final NamedResource subject = ax.getSubject();
        final Assertion assertion = createAssertion();
        final ContainerDescriptor desc = switch (containerType) {
            case SEQ -> ContainerDescriptor.seqDescriptor(subject, assertion, getAttributeWriteContext());
            case ALT -> ContainerDescriptor.altDescriptor(subject, assertion, getAttributeWriteContext());
            case BAG -> ContainerDescriptor.bagDescriptor(subject, assertion, getAttributeWriteContext());
        };
        mapper.loadRdfContainer(desc).forEach(super::addAxiomValue);
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) {
        final Object value = extractFieldValueFromInstance(instance);
        assert value instanceof Collection || value == null;
        final Collection<?> valueCollection = (Collection<?>) value;
        final ContainerValueDescriptor<NamedResource> valueDescriptor = createContainerValueDescriptor(instance);
        if (valueCollection != null && !valueCollection.isEmpty()) {
            addValuesToDescriptor(valueCollection, valueDescriptor);
        }
        valueBuilder.addContainerValues(valueDescriptor);
    }

    private ContainerValueDescriptor<NamedResource> createContainerValueDescriptor(X owner) {
        final NamedResource subject = NamedResource.create(EntityPropertiesUtils.getIdentifier(owner, et));
        return switch (containerType) {
            case SEQ ->
                    ContainerValueDescriptor.seqValueDescriptor(subject, createAssertion(), getAttributeWriteContext());
            case ALT ->
                    ContainerValueDescriptor.altValueDescriptor(subject, createAssertion(), getAttributeWriteContext());
            case BAG ->
                    ContainerValueDescriptor.bagValueDescriptor(subject, createAssertion(), getAttributeWriteContext());
        };
    }

    private <T> void addValuesToDescriptor(Collection<T> valueCollection,
                                           ContainerValueDescriptor<NamedResource> valueDescriptor) {
        final Class<?> elemType = attribute.getBindableJavaType();
        if (IdentifierTransformer.isValidIdentifierType(elemType)) {
            valueCollection.stream().filter(Objects::nonNull)
                           .forEach(item -> valueDescriptor.addValue(NamedResource.create(IdentifierTransformer.valueAsUri(item))));
        } else if (elemType.isEnum()) {
            assert attribute.getConverter() != null;
            valueCollection.stream().filter(Objects::nonNull).forEach(
                    item -> valueDescriptor.addValue((NamedResource) attribute.getConverter()
                                                                              .convertToAxiomValue(item)));
        } else {
            final EntityType<T> et = (EntityType<T>) mapper.getEntityType(elemType);
            valueCollection.stream().filter(Objects::nonNull).forEach(item -> {
                if (referenceSavingResolver.shouldSaveReferenceToItem(item, getAttributeValueContexts())) {
                    final URI valId = EntityPropertiesUtils.getIdentifier(item, et);
                    assert valId != null;
                    valueDescriptor.addValue(NamedResource.create(valId));
                } else {
                    referenceSavingResolver.registerPendingReference(valueDescriptor.getOwner(), createAssertion(), item, getAttributeWriteContext());
                }
            });
        }
    }

    @Override
    Set<Axiom<?>> buildAxiomsFromInstance(X instance) {
        throw new UnsupportedOperationException("Method not supported for RDF containers.");
    }
}

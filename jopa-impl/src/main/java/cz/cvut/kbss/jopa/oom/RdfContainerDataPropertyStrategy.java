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
import cz.cvut.kbss.ontodriver.descriptor.ContainerDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ContainerValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.util.Collection;
import java.util.Objects;
import java.util.Set;

/**
 * Mapping for RDF containers with data property values.
 *
 * @param <X> Entity type
 */
class RdfContainerDataPropertyStrategy<X> extends PluralDataPropertyStrategy<X> {

    private final RDFContainerType containerType;

    RdfContainerDataPropertyStrategy(EntityType<X> et,
                                     RdfContainerAttributeImpl<? super X, ?, ?> att,
                                     Descriptor attributeDescriptor,
                                     EntityMappingHelper mapper) {
        super(et, att, attributeDescriptor, mapper);
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
        if (valueCollection == null || valueCollection.isEmpty()) {
            return;
        }
        final ContainerValueDescriptor valueDescriptor = createValueDescriptor(instance);
        valueCollection.stream()
                       .filter(Objects::nonNull)
                       .map(converter::convertToAxiomValue)
                       .forEach(valueDescriptor::addValue);
        valueBuilder.addContainerValues(valueDescriptor);
    }

    private ContainerValueDescriptor<?> createValueDescriptor(X instance) {
        final NamedResource owner = NamedResource.create(EntityPropertiesUtils.getIdentifier(instance, et));
        return switch (containerType) {
            case SEQ ->
                    ContainerValueDescriptor.seqValueDescriptor(owner, createAssertion(), getAttributeWriteContext());
            case ALT ->
                    ContainerValueDescriptor.altValueDescriptor(owner, createAssertion(), getAttributeWriteContext());
            case BAG ->
                    ContainerValueDescriptor.bagValueDescriptor(owner, createAssertion(), getAttributeWriteContext());
        };
    }

    @Override
    Set<Axiom<?>> buildAxiomsFromInstance(X instance) {
        throw new UnsupportedOperationException("Method not supported for RDF containers.");
    }
}

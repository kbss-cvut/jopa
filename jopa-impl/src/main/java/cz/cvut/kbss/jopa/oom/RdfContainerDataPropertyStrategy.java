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

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.RDFContainerAttribute;
import cz.cvut.kbss.jopa.model.metamodel.RdfContainerAttributeImpl;
import cz.cvut.kbss.ontodriver.model.Axiom;

import java.util.Optional;
import java.util.Set;

/**
 * Mapping for RDF containers with data property values.
 *
 * @param <X> Entity type
 */
class RdfContainerDataPropertyStrategy<X> extends PluralDataPropertyStrategy<X> {

    RdfContainerDataPropertyStrategy(EntityType<X> et, RdfContainerAttributeImpl<? super X, ?, ?> att,
                                     Descriptor attributeDescriptor, EntityMappingHelper mapper) {
        super(et, att, attributeDescriptor, mapper);
    }

    @Override
    void addAxiomValue(Axiom<?> ax) {
        new RdfContainerObjectPropertyStrategy.RdfContainerLoader<>((RDFContainerAttribute<?, ?, ?>) attribute, mapper, this).loadFromAxiom(ax, super::addAxiomValue);
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) {
        final RdfContainerObjectPropertyStrategy.RdfContainerSaver<X> saver = new RdfContainerObjectPropertyStrategy.RdfContainerSaver<>(this, (RDFContainerAttribute<?, ?, ?>) attribute);
        saver.save(instance, valueBuilder, (item, _) -> Optional.of(converter.convertToAxiomValue(item)));
    }

    @Override
    Set<Axiom<?>> buildAxiomsFromInstance(X instance) {
        throw new UnsupportedOperationException("Method not supported for RDF containers.");
    }
}

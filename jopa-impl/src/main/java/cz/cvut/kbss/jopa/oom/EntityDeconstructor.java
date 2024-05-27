/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.QueryAttribute;
import cz.cvut.kbss.jopa.oom.exception.EntityDeconstructionException;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;

class EntityDeconstructor {

    private final EntityMappingHelper mapper;
    private ReferenceSavingResolver referenceSavingResolver;

    EntityDeconstructor(ObjectOntologyMapperImpl mapper) {
        this.mapper = mapper;
    }

    void setReferenceSavingResolver(ReferenceSavingResolver referenceSavingResolver) {
        this.referenceSavingResolver = referenceSavingResolver;
    }

    <T> AxiomValueGatherer mapEntityToAxioms(URI identifier, T entity, EntityType<T> et, Descriptor descriptor) {
        assert identifier != null;

        final AxiomValueGatherer valueBuilder = createAxiomValueBuilder(identifier, descriptor);
        try {
            for (FieldSpecification<? super T, ?> att : et.getFieldSpecifications()) {
                if (att instanceof QueryAttribute) {
                    // Skip query based attributes, they are not mapped to axioms
                    continue;
                }
                addAssertions(entity, et, att, descriptor, valueBuilder);
            }

        } catch (IllegalArgumentException e) {
            throw new EntityDeconstructionException(e);
        }
        return valueBuilder;
    }

    private static AxiomValueGatherer createAxiomValueBuilder(URI identifier, Descriptor descriptor) {
        return new AxiomValueGatherer(NamedResource.create(identifier), descriptor.getSingleContext().orElse(null));
    }

    private <T> void addAssertions(T entity, EntityType<T> et,
                                   FieldSpecification<? super T, ?> fieldSpec, Descriptor entityDescriptor,
                                   final AxiomValueGatherer valueBuilder) {
        final FieldStrategy<? extends FieldSpecification<? super T, ?>, T> fs = FieldStrategy
                .createFieldStrategy(et, fieldSpec, entityDescriptor, mapper);
        fs.setReferenceSavingResolver(referenceSavingResolver);
        fs.buildAxiomValuesFromInstance(entity, valueBuilder);
    }

    <T> AxiomValueGatherer mapFieldToAxioms(URI primaryKey, T entity, FieldSpecification<? super T, ?> fieldSpec,
                                            EntityType<T> et, Descriptor descriptor) {
        final AxiomValueGatherer valueBuilder = createAxiomValueBuilder(primaryKey, descriptor);
        addAssertions(entity, et, fieldSpec, descriptor, valueBuilder);
        return valueBuilder;
    }
}

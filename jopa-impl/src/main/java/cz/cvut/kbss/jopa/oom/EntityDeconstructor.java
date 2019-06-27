/**
 * Copyright (C) 2019 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.oom.exceptions.EntityDeconstructionException;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.lang.reflect.Field;
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

    <T> AxiomValueGatherer mapEntityToAxioms(URI identifier, T entity, EntityType<T> et,
                                             Descriptor descriptor) {
        assert identifier != null;

        final AxiomValueGatherer valueBuilder = createAxiomValueBuilder(identifier, descriptor);
        try {
            for (FieldSpecification<? super T, ?> att : et.getFieldSpecifications()) {
                addAssertions(entity, et, att, descriptor, valueBuilder);
            }

        } catch (IllegalArgumentException | IllegalAccessException e) {
            throw new EntityDeconstructionException(e);
        }
        return valueBuilder;
    }

    private static AxiomValueGatherer createAxiomValueBuilder(URI primaryKey, Descriptor descriptor) {
        return new AxiomValueGatherer(NamedResource.create(primaryKey), descriptor.getContext());
    }

    private <T> void addAssertions(T entity, EntityType<T> et,
                                   FieldSpecification<? super T, ?> fieldSpec, Descriptor entityDescriptor,
                                   final AxiomValueGatherer valueBuilder) throws IllegalAccessException {
        final FieldStrategy<? extends FieldSpecification<? super T, ?>, T> fs = FieldStrategy
                .createFieldStrategy(et, fieldSpec, entityDescriptor, mapper);
        fs.setReferenceSavingResolver(referenceSavingResolver);
        fs.buildAxiomValuesFromInstance(entity, valueBuilder);
    }

    <T> AxiomValueGatherer mapFieldToAxioms(URI primaryKey, T entity, Field field, EntityType<T> et,
                                            Descriptor descriptor) {
        final FieldSpecification<? super T, ?> fieldSpec = et
                .getFieldSpecification(field.getName());
        final AxiomValueGatherer valueBuilder = createAxiomValueBuilder(primaryKey, descriptor);
        try {
            addAssertions(entity, et, fieldSpec, descriptor, valueBuilder);
        } catch (IllegalAccessException e) {
            throw new EntityDeconstructionException(e);
        }
        return valueBuilder;
    }
}

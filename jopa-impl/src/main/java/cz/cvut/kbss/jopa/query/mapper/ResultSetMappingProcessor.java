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
package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.exception.SparqlResultMappingException;
import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.model.metamodel.*;
import cz.cvut.kbss.jopa.query.ResultSetMappingManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Builds mappers for {@link SparqlResultSetMapping} instances discovered on classpath.
 */
public class ResultSetMappingProcessor {

    private static final Logger LOG = LoggerFactory.getLogger(ResultSetMappingProcessor.class);

    private final ResultSetMappingManager manager = new ResultSetMappingManager();

    private final MetamodelBuilder metamodelBuilder;

    public ResultSetMappingProcessor(MetamodelBuilder metamodelBuilder) {
        this.metamodelBuilder = metamodelBuilder;
    }

    /**
     * Builds result set mapper for the specified mapping.
     *
     * @param mapping Mapping configuration
     */
    public void buildMapper(SparqlResultSetMapping mapping) {
        Objects.requireNonNull(mapping);
        final ResultRowMapper rowMapper = new ResultRowMapper(mapping.name());
        buildVariableMappers(mapping, rowMapper);
        buildConstructorMappers(mapping, rowMapper);
        buildEntityMappers(mapping, rowMapper);
        manager.addMapper(rowMapper.getName(), rowMapper);
    }

    private static void buildVariableMappers(SparqlResultSetMapping mapping, ResultRowMapper parent) {
        for (VariableResult vr : mapping.variables()) {
            parent.addMapper(new VariableResultMapper(vr));
        }
    }

    private static void buildConstructorMappers(SparqlResultSetMapping mapping, ResultRowMapper parent) {
        for (ConstructorResult cr : mapping.classes()) {
            final ConstructorResultMapper mapper = new ConstructorResultMapper(cr.targetClass());
            for (VariableResult vr : cr.variables()) {
                mapper.addParameterMapper(new VariableResultMapper(vr));
            }
            parent.addMapper(mapper);
        }
    }

    private void buildEntityMappers(SparqlResultSetMapping mapping, ResultRowMapper parent) {
        for (EntityResult er : mapping.entities()) {
            final EntityType<?> et = getTargetType(er);
            final EntityResultMapper<?> etMapper = new EntityResultMapper<>(et);
            generateFieldMappersForFieldResults(er, et, etMapper);
            generateFieldMappersForUnconfiguredFields(et, er).forEach(etMapper::addFieldMapper);
            parent.addMapper(etMapper);
        }
    }

    private EntityType<?> getTargetType(EntityResult er) {
        final AbstractIdentifiableType<?> targetType = metamodelBuilder.entity(er.entityClass());
        if (!(targetType instanceof EntityType)) {
            throw new SparqlResultMappingException(
                    "Type " + er.entityClass() +
                            " is not a known entity type and cannot be used as @EntityResult target class.");
        }
        return (EntityType<?>) targetType;
    }

    private static void generateFieldMappersForFieldResults(EntityResult er, EntityType<?> et,
                                                     EntityResultMapper<?> etMapper) {
        for (FieldResult fr : er.fields()) {
            try {
                final FieldSpecification<?, ?> fieldSpec = et.getFieldSpecification(fr.name());
                createFieldMapper(fr, fieldSpec).ifPresent(etMapper::addFieldMapper);
            } catch (IllegalArgumentException e) {
                throw new SparqlResultMappingException(e);
            }
        }
    }

    private static Optional<FieldResultMapper> createFieldMapper(FieldResult fr, FieldSpecification<?, ?> fieldSpec) {
        if (fieldSpec.isCollection()) {
            LOG.warn(
                    "Mapping of plural attributes via @FieldResult is not supported. Check the definition of {}.",
                    fr);
            return Optional.empty();
        }
        if (fieldSpec instanceof Attribute &&
                ((Attribute) fieldSpec).getPersistentAttributeType() == Attribute.PersistentAttributeType.OBJECT) {
            return Optional.of(new ObjectPropertyFieldResultMapper(fr, fieldSpec));
        } else {
            return Optional.of(new FieldResultMapper(fr, fieldSpec));
        }
    }

    private static List<FieldResultMapper> generateFieldMappersForUnconfiguredFields(EntityType<?> et, EntityResult er) {
        final Set<String> configuredFields = new HashSet<>(er.fields().length);
        for (FieldResult fr : er.fields()) {
            configuredFields.add(fr.name());
        }
        return et.getFieldSpecifications().stream()
                 .filter(fs -> !configuredFields.contains(fs.getName()) && !fs.isCollection())
                 .map(ResultSetMappingProcessor::createFieldMapper)
                 .collect(Collectors.toList());
    }

    private static FieldResultMapper createFieldMapper(FieldSpecification<?, ?> fieldSpec) {
        if (fieldSpec instanceof Attribute &&
                ((Attribute) fieldSpec).getPersistentAttributeType() == Attribute.PersistentAttributeType.OBJECT) {
            return new ObjectPropertyFieldResultMapper(fieldSpec);
        } else {
            return new FieldResultMapper(fieldSpec);
        }
    }

    public ResultSetMappingManager getManager() {
        return manager;
    }
}

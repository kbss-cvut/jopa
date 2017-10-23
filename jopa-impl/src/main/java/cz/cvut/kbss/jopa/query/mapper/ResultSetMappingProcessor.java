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
package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.exception.SparqlResultMappingException;
import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.model.metamodel.AbstractIdentifiableType;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.MetamodelBuilder;
import cz.cvut.kbss.jopa.query.ResultSetMappingManager;

import java.util.Objects;

/**
 * Builds mappers for {@link SparqlResultSetMapping} instances discovered on classpath.
 */
public class ResultSetMappingProcessor {

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

    private void buildVariableMappers(SparqlResultSetMapping mapping, ResultRowMapper parent) {
        for (VariableResult vr : mapping.variables()) {
            parent.addMapper(new VariableResultMapper(vr));
        }
    }

    private void buildConstructorMappers(SparqlResultSetMapping mapping, ResultRowMapper parent) {
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
            for (FieldResult fr : er.fields()) {
                try {
                    etMapper.addFieldMapper(new FieldResultMapper(fr, et.getFieldSpecification(fr.name())));
                } catch (IllegalArgumentException e) {
                    throw new SparqlResultMappingException(e);
                }
            }
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

    public ResultSetMappingManager getManager() {
        return manager;
    }
}

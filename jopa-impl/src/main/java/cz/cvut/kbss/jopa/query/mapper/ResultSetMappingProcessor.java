/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.model.annotations.ConstructorResult;
import cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping;
import cz.cvut.kbss.jopa.model.annotations.VariableResult;
import cz.cvut.kbss.jopa.query.ResultSetMappingManager;

import java.util.Objects;

/**
 * Builds mappers for {@link SparqlResultSetMapping} instances discovered on classpath.
 */
public class ResultSetMappingProcessor {

    private final ResultSetMappingManager manager = new ResultSetMappingManager();

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

    public ResultSetMappingManager getManager() {
        return manager;
    }
}

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

import cz.cvut.kbss.jopa.exception.SparqlResultMappingException;
import cz.cvut.kbss.jopa.model.annotations.VariableResult;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.jopa.utils.DatatypeTransformer;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

/**
 * Maps SPARQL query result to target value based on a {@link VariableResult} configuration.
 */
class VariableResultMapper implements SparqlResultMapper {

    private final String name;
    private final Class<?> targetType;

    VariableResultMapper(VariableResult mapping) {
        this.name = mapping.name();
        this.targetType = mapping.type();
    }

    String getName() {
        return name;
    }

    Class<?> getTargetType() {
        return targetType;
    }

    /**
     * Maps value from the current line of the specified result set according to the {@link VariableResult}
     * configuration represented by this instance.
     *
     * @param resultSet Query result set to read
     * @param uow
     * @return The mapped value
     */
    @Override
    public Object map(ResultSet resultSet, UnitOfWorkImpl uow) {
        try {
            final Object value = resultSet.getObject(name);
            if (!void.class.equals(targetType)) {
                return DatatypeTransformer.transform(value, targetType);
            }
            return value;
        } catch (OntoDriverException e) {
            throw new SparqlResultMappingException(e);
        }
    }
}

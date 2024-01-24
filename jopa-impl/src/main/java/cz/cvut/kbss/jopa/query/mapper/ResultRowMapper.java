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

import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Represents a single {@link cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping} instance.
 * <p>
 * This instance can contain multiple {@link SparqlResultMapper} instances, representing the individual mappings
 * specified by the {@link cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping}.
 */
public class ResultRowMapper implements SparqlResultMapper {

    private final String name;

    private final List<SparqlResultMapper> rowMappers = new ArrayList<>();

    public ResultRowMapper(String name) {
        this.name = name;
    }

    /**
     * Gets the name of the result set mapping represented by this mapper.
     *
     * @return Mapping name
     */
    public String getName() {
        return name;
    }

    List<SparqlResultMapper> getRowMappers() {
        return Collections.unmodifiableList(rowMappers);
    }

    void addMapper(SparqlResultMapper mapper) {
        rowMappers.add(mapper);
    }

    @Override
    public Object map(ResultRow resultRow, UnitOfWork uow) {
        if (rowMappers.size() == 1) {
            return rowMappers.get(0).map(resultRow, uow);
        }
        final Object[] result = new Object[rowMappers.size()];
        int i = 0;
        for (SparqlResultMapper m : rowMappers) {
            result[i++] = m.map(resultRow, uow);
        }
        return result;
    }
}

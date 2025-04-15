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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.query.QueryHolder;
import cz.cvut.kbss.jopa.query.mapper.SparqlResultMapper;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;

/**
 * Implementation of query using result set mapping configuration.
 */
public class ResultSetMappingQuery extends QueryImpl {

    private final SparqlResultMapper mapper;
    private final UnitOfWork uow;

    public ResultSetMappingQuery(QueryHolder query, ConnectionWrapper connection, SparqlResultMapper mapper,
                                 UnitOfWork uow) {
        super(query, connection);
        this.mapper = mapper;
        this.uow = uow;
    }

    @Override
    Object extractRow(ResultRow resultRow) {
        return mapper.map(resultRow, uow);
    }
}

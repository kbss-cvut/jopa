/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.query.QueryHolder;
import cz.cvut.kbss.jopa.query.mapper.SparqlResultMapper;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;

/**
 * Implementation of query using result set mapping configuration.
 */
public class ResultSetMappingQuery extends QueryImpl {

    private final SparqlResultMapper mapper;
    private final UnitOfWorkImpl uow;

    public ResultSetMappingQuery(QueryHolder query, ConnectionWrapper connection, SparqlResultMapper mapper,
                                 UnitOfWorkImpl uow) {
        super(query, connection);
        this.mapper = mapper;
        this.uow = uow;
    }

    @Override
    Object extractRow(ResultRow resultRow) {
        return mapper.map(resultRow, uow);
    }
}

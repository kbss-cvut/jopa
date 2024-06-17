/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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

/**
 * Maps SPARQL result set value (a subset of the current row) to output based on a {@link
 * cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping} configuration.
 */
@FunctionalInterface
public interface SparqlResultMapper {

    /**
     * Maps (a subset of) the specified current result row to output based on this instance's configuration.
     *
     * @param resultRow The result row to map
     * @param uow       Current persistence context
     * @return Result of the mapping
     */
    Object map(ResultRow resultRow, UnitOfWork uow);
}

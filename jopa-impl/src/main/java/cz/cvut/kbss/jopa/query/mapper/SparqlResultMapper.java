/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;

/**
 * Maps SPARQL result set value (a subset of the current row) to output based on a {@link
 * cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping} configuration.
 */
public interface SparqlResultMapper {

    /**
     * Maps (a subset of) the specified current result row to output based on this instance's configuration.
     *
     * @param resultRow The result row to map
     * @param uow       Current persistence context
     * @return Result of the mapping
     */
    Object map(ResultRow resultRow, UnitOfWorkImpl uow);
}

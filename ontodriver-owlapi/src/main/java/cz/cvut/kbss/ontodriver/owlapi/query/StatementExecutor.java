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
package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.owlapi.exception.OwlapiDriverException;

public interface StatementExecutor {

    /**
     * Executes the specified read-only query.
     *
     * @param query     The query to execute
     * @return Query result
     * @throws OwlapiDriverException When storage access error occurs.
     */
    ResultSet executeQuery(QuerySpecification query) throws OwlapiDriverException;

    /**
     * Executes the specified update query.
     * <p>
     * This query may modify the state of the ontology.
     *
     * @param query Specification of the update query to execute
     * @throws OwlapiDriverException When storage access error occurs
     */
    void executeUpdate(QuerySpecification query) throws OwlapiDriverException;
}

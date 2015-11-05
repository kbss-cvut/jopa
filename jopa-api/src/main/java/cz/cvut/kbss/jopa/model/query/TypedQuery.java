/**
 * Copyright (C) 2011 Czech Technical University in Prague
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

package cz.cvut.kbss.jopa.model.query;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.exceptions.TransactionRequiredException;

import java.util.List;

public interface TypedQuery<ResultElement> extends Query<ResultElement> {
    /**
     * Execute a SELECT query and return the query results as a typed List.
     *
     * @return a list of the results
     * @throws IllegalStateException        if called for a Java Persistence query language UPDATE or DELETE statement
     * @throws TransactionRequiredException if a lock mode has been set and there is no transaction
     * @throws OWLPersistenceException      if the query execution exceeds the query timeout value set and the
     *                                      transaction is rolled back
     */
    @Override
    List<ResultElement> getResultList();
}

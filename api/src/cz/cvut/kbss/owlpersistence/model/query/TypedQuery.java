/**
 * Copyright (C) 2011 Czech Technical University in Prague
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

package cz.cvut.kbss.owlpersistence.model.query;

import java.util.List;

import javax.transaction.TransactionRequiredException;

public interface TypedQuery<ResultElement> extends Query<ResultElement> {
	/**
	 * Execute a SELECT query and return the query results as a typed List.
	 * 
	 * @return a list of the results
	 * @throws IllegalStateException
	 *             if called for a Java
	 * 
	 *             Persistence query language UPDATE or DELETE statement
	 * @throws QueryTimeoutException
	 *             if the query execution exceeds
	 * 
	 *             the query timeout value set and only the statement is
	 * 
	 *             rolled back
	 * @throws TransactionRequiredException
	 *             if a lock mode has
	 * 
	 *             been set and there is no transaction
	 * @throws PessimisticLockException
	 *             if pessimistic locking
	 * 
	 *             fails and the transaction is rolled back
	 * @throws LockTimeoutException
	 *             if pessimistic locking
	 * 
	 *             fails and only the statement is rolled back
	 * @throws PersistenceException
	 *             if the query execution exceeds
	 * 
	 *             the query timeout value set and the transaction
	 * 
	 *             is rolled back
	 */
	List<ResultElement> getResultList();

}

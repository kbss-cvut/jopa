/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.sesame.connector;

import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.QueryEvaluationException;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;

import java.util.List;

/**
 * This class wraps the Sesame TupleQueryResult returned by QueryExecutor to be
 * able to close the repository connection once the result is closed.
 * 
 */
class QueryResult implements TupleQueryResult {

	private final TupleQueryResult result;
	private final RepositoryConnection connection;

	QueryResult(TupleQueryResult result, RepositoryConnection connection) {
		this.result = result;
		this.connection = connection;
	}

	@Override
	public void close() throws QueryEvaluationException {
		result.close();
		try {
			connection.close();
		} catch (RepositoryException e) {
			throw new QueryEvaluationException(e);
		}
	}

	@Override
	public boolean hasNext() throws QueryEvaluationException {
		return result.hasNext();
	}

	@Override
	public BindingSet next() throws QueryEvaluationException {
		return result.next();
	}

	@Override
	public void remove() throws QueryEvaluationException {
		result.remove();
	}

	@Override
	public List<String> getBindingNames() throws QueryEvaluationException {
		return result.getBindingNames();
	}
}

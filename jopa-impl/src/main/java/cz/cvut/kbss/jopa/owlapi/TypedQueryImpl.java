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

package cz.cvut.kbss.jopa.owlapi;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.exceptions.NoUniqueResultException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class TypedQueryImpl<T> implements TypedQuery<T> {

	private final String query;
	private final URI contextUri;
	private final boolean sparql;
	private final Class<T> classT;
	private final UnitOfWork uow;
	private final Connection connection;
	private final boolean useBackupOntology;

	private int maxResults;

	// sparql=false -> abstract syntax
	public TypedQueryImpl(final String query, final Class<T> classT, final URI contextUri,
			final boolean sparql, final UnitOfWork uow, final Connection connection) {
		if (query == null || contextUri == null || classT == null || uow == null
				|| connection == null) {
			throw new NullPointerException();
		}
		this.query = query;
		this.contextUri = contextUri;
		this.sparql = sparql;
		this.classT = classT;
		this.uow = uow;
		this.connection = connection;
		this.useBackupOntology = uow.useBackupOntologyForQueryProcessing();
		this.maxResults = Integer.MAX_VALUE;
	}

	@Override
	public List<T> getResultList() {
		if (!sparql) {
			throw new NotYetImplementedException();
		}

		if (maxResults == 0) {
			return Collections.emptyList();
		}

		List<T> list;
		try {
			list = getResultListImpl(maxResults);
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException("Exeption caught when evaluating query " + query, e);
		}

		return list;
	}

	@Override
	public T getSingleResult() {
		try {
			// call it with maxResults = 2 just to see whether there are
			// multiple results
			final List<T> res = getResultListImpl(2);
			if (res.isEmpty()) {
				throw new NoResultException("No result found for query " + query);
			}
			if (res.size() > 1) {
				throw new NoUniqueResultException("Multiple results found for query " + query);
			}
			return res.get(0);
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException("Exception caught when evaluating query " + query, e);
		}
	}

	@Override
	public TypedQuery<T> setMaxResults(int maxResults) {
		if (maxResults < 0) {
			throw new IllegalArgumentException(
					"Cannot set maximum number of results to less than 0.");
		}
		this.maxResults = maxResults;
		return this;
	}

	@Override
	public int getMaxResults() {
		return maxResults;
	}

	private List<T> getResultListImpl(int maxResults) throws OntoDriverException {
		assert maxResults > 0;
		final Statement stmt = connection.createStatement();
		if (useBackupOntology) {
			stmt.setUseBackupOntology();
		} else {
			stmt.setUseTransactionalOntology();
		}
		final ResultSet rs = stmt.executeQuery(query, contextUri);
		try {
			final List<T> res = new ArrayList<T>();
			// TODO register this as observer on the result set so that
			// additional results can be loaded asynchronously
			int cnt = 0;
			while (rs.hasNext() && cnt < maxResults) {
				rs.next();
				final OWLNamedIndividual ind = rs.getObject(0, OWLNamedIndividual.class);

				final T entity = uow.readObject(classT, ind.getIRI());
				if (entity == null) {
					throw new OWLPersistenceException(
							"Fatal error, unable to load entity for primary key already found by query "
									+ query);
				}
				res.add(entity);
				cnt++;
			}
			return res;
		} finally {
			rs.close();
		}
	}
}

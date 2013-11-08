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

import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLObject;

import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.exceptions.NoUniqueResultException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class QueryImpl implements Query<List<String>> {

	private final String query;
	private final URI contextUri;
	private final boolean sparql;
	private final Connection connection;

	private int maxResults;
	private boolean useBackupOntology;

	// sparql=false -> abstract syntax
	public QueryImpl(final String s, final URI contextUri, final boolean sparql,
			final Connection connection) {
		if (s == null || contextUri == null || connection == null) {
			throw new NullPointerException();
		}
		this.useBackupOntology = false;
		this.query = s;
		this.contextUri = contextUri;
		this.sparql = sparql;
		this.connection = connection;
		this.maxResults = Integer.MAX_VALUE;
	}

	public List<List<String>> getResultList() {
		if (!sparql) {
			throw new NotYetImplementedException();
		}
		try {
			if (maxResults == 0) {
				return Collections.emptyList();
			}
			return getResultListImpl(maxResults);
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException("Exception caught when evaluating query " + query, e);
		}

		// TODO This will be in the result set
		// final List<List<String>> list = new ArrayList<List<String>>();
		//
		// final QueryResult<OWLObject> l = OWL2QueryEngine.<OWLObject>
		// exec(query, r);
		//
		// for (final Iterator<ResultBinding<OWLObject>> i = l.iterator(); i
		// .hasNext();) {
		// final List<String> solution = new ArrayList<String>();
		// list.add(solution);
		//
		// final ResultBinding<OWLObject> b = i.next();
		//
		// for (final Variable<OWLObject> v : l.getResultVars()) {
		// final OWLObject o = b.get(v).asGroundTerm().getWrappedObject();
		//
		// if (o instanceof OWLLiteral) {
		// solution.add(""+DatatypeTransformer.transform((OWLLiteral) o));
		// } else if (o instanceof OWLEntity) {
		// solution.add(((OWLEntity) o).getIRI().toString());
		// }
		// }
		// }
		// return list;
	}

	public List<String> getSingleResult() {
		try {
			// Call it with maxResults = 2 just to see whether there are more
			final List<List<String>> list = getResultListImpl(2);
			if (list.isEmpty()) {
				throw new NoResultException("No result found for querytransactional " + query);
			}
			if (list.size() > 1) {
				throw new NoUniqueResultException("Multiple results found for query " + query);
			}
			return list.get(0);
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException("Exception caught when evaluating query " + query, e);
		}
	}

	public Query<List<String>> setMaxResults(int maxResults) {
		if (maxResults < 0) {
			throw new IllegalArgumentException(
					"Cannot set maximum number of results to less than 0.");
		}
		this.maxResults = maxResults;
		return this;
	}

	/**
	 * Sets ontology used for processing of this query. </p>
	 * 
	 * @param useTransactional
	 *            If true, the backup (central) ontology is used, otherwise the
	 *            transactional ontology is used (default)
	 */
	public void setUseBackupOntology(boolean useBackupOntology) {
		this.useBackupOntology = useBackupOntology;
	}

	private List<List<String>> getResultListImpl(int maxResults) throws OntoDriverException {
		assert maxResults > 0;
		final Statement stmt = connection.createStatement();
		if (useBackupOntology) {
			stmt.setUseBackupOntology();
		} else {
			stmt.setUseTransactionalOntology();
		}
		final ResultSet rs = stmt.executeQuery(query, contextUri);
		try {
			final int cols = rs.getColumnCount();
			int cnt = 0;
			final List<List<String>> res = new ArrayList<List<String>>();
			while (rs.hasNext() && cnt < maxResults) {
				rs.next();
				final List<String> row = new ArrayList<String>(cols);
				res.add(row);
				for (int i = 0; i < cols; i++) {
					final OWLObject ob = rs.getObject(i, OWLObject.class);
					if (ob instanceof OWLLiteral) {
						final String s = DatatypeTransformer.transform((OWLLiteral) ob).toString();
						row.add(s);
					} else if (ob instanceof OWLEntity) {
						row.add(((OWLEntity) ob).getIRI().toString());
					}
				}
				cnt++;
			}
			return res;
		} finally {
			rs.close();
		}
	}
}

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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLObject;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.owlapi.DatatypeTransformer;
import cz.cvut.kbss.owl2query.engine.OWL2QueryEngine;
import cz.cvut.kbss.owl2query.model.OWL2Ontology;
import cz.cvut.kbss.owl2query.model.QueryResult;
import cz.cvut.kbss.owl2query.model.ResultBinding;
import cz.cvut.kbss.owl2query.model.Variable;

public class QueryImpl implements Query<List<String>> {

	final String s;
	final boolean sparql;
	final OWL2Ontology<OWLObject> r;
	final EntityManager em;
	
	// sparql=false -> abstract syntax
	public QueryImpl(final String s, OWL2Ontology<OWLObject> r,
			final boolean sparql, final EntityManager em) {
		this.s = s;
		this.r = r;
		this.sparql = sparql;
		this.em = em;
	}

	
	public List<List<String>> getResultList() {
		if (!sparql) {
			throw new NotYetImplementedException();
		}

		final List<List<String>> list = new ArrayList<List<String>>();

		final QueryResult<OWLObject> l = OWL2QueryEngine.<OWLObject> exec(s, r);

		for (final Iterator<ResultBinding<OWLObject>> i = l.iterator(); i
				.hasNext();) {
			final List<String> solution = new ArrayList<String>();
			list.add(solution);

			final ResultBinding<OWLObject> b = i.next();
			
			for (final Variable<OWLObject> v : l.getResultVars()) {
				final OWLObject o = b.get(v).asGroundTerm().getWrappedObject();

				if (o instanceof OWLLiteral) {
					solution.add(""+DatatypeTransformer.transform((OWLLiteral) o));
				} else if (o instanceof OWLEntity) {
					solution.add(((OWLEntity) o).getIRI().toString());
				}
			}
		}
		return list;
	}

	
	public List<String> getSingleResult() {
		List<List<String>> list = getResultList();
		
		if ( list!= null && !list.isEmpty() ) {
			return list.iterator().next();
		} else {
			return Collections.emptyList();
		}
		
	}

	
	public Query<List<String>> setMaxResults(int maxResult) {
		// TODO Auto-generated method stub
		return null;
	}

}

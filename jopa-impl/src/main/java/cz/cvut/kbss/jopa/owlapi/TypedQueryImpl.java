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
import java.util.Iterator;
import java.util.List;

import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.owl2query.engine.OWL2QueryEngine;
import cz.cvut.kbss.owl2query.model.OWL2Ontology;
import cz.cvut.kbss.owl2query.model.QueryResult;
import cz.cvut.kbss.owl2query.model.ResultBinding;

public class TypedQueryImpl<T> implements TypedQuery<T> {

	final String s;
	final boolean sparql;
	final OWL2Ontology<OWLObject> r;
	final Class<T> classT;
	final EntityManager em;

	// sparql=false -> abstract syntax
	public TypedQueryImpl(final String s, final Class<T> classT,
			OWL2Ontology<OWLObject> r, final boolean sparql,
			final EntityManager em) {
		this.s = s;
		this.r = r;
		this.sparql = sparql;
		this.classT = classT;
		this.em = em;
	}

	
	public List<T> getResultList() {
		if (!sparql) {
			throw new NotYetImplementedException();
		}

		final List<T> list = new ArrayList<T>();

		final QueryResult<OWLObject> l = OWL2QueryEngine.<OWLObject> exec(s, r);

		for (final Iterator<ResultBinding<OWLObject>> i = l.iterator(); i
				.hasNext();) {

			if (classT != null) {
				final ResultBinding<OWLObject> b = i.next();

				final OWLNamedIndividual o = (OWLNamedIndividual) b.get(
						b.keySet().iterator().next()).asGroundTerm()
						.getWrappedObject();

				list.add(em.find(classT, o.getIRI().toString()));
			}
		}

		return list;
	}

	
	public T getSingleResult() {
		// TODO Auto-generated method stub
		return null;
	}

	
	public Query setMaxResults(int maxResult) {
		// TODO Auto-generated method stub
		return null;
	}

}

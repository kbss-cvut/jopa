package cz.cvut.kbss.owlpersistence.owlapi;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;

import cz.cvut.kbss.owl2query.simpleversion.engine.OWL2QueryEngine;
import cz.cvut.kbss.owl2query.simpleversion.model.OWL2Ontology;
import cz.cvut.kbss.owl2query.simpleversion.model.QueryResult;
import cz.cvut.kbss.owl2query.simpleversion.model.ResultBinding;
import cz.cvut.kbss.owlpersistence.model.EntityManager;
import cz.cvut.kbss.owlpersistence.model.query.Query;
import cz.cvut.kbss.owlpersistence.model.query.TypedQuery;

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

	@Override
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

	@Override
	public Object getSingleResult() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Query setMaxResults(int maxResult) {
		// TODO Auto-generated method stub
		return null;
	}

}

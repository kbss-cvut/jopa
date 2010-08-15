package cz.cvut.kbss.owlpersistence.owlapi;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLObject;

import cz.cvut.kbss.owl2query.simpleversion.engine.OWL2QueryEngine;
import cz.cvut.kbss.owl2query.simpleversion.model.OWL2Ontology;
import cz.cvut.kbss.owl2query.simpleversion.model.QueryResult;
import cz.cvut.kbss.owl2query.simpleversion.model.ResultBinding;
import cz.cvut.kbss.owl2query.simpleversion.model.Variable;
import cz.cvut.kbss.owlpersistence.model.EntityManager;
import cz.cvut.kbss.owlpersistence.model.query.Query;

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

	@Override
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

	@Override
	public List<String> getSingleResult() {
		List<List<String>> list = getResultList();
		
		if ( list!= null && !list.isEmpty() ) {
			return list.iterator().next();
		} else {
			return Collections.emptyList();
		}
		
	}

	@Override
	public Query<List<String>> setMaxResults(int maxResult) {
		// TODO Auto-generated method stub
		return null;
	}

}

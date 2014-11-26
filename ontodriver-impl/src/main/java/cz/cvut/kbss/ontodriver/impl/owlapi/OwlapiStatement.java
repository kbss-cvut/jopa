package cz.cvut.kbss.ontodriver.impl.owlapi;

import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import cz.cvut.kbss.ontodriver.AbstractStatement;
import cz.cvut.kbss.ontodriver.JopaStatement;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exceptions.QueryExecutionException;
import cz.cvut.kbss.owl2query.engine.OWL2QueryEngine;
import cz.cvut.kbss.owl2query.model.QueryResult;
import cz.cvut.kbss.owl2query.model.owlapi.OWLAPIv3OWL2Ontology;

public class OwlapiStatement extends AbstractStatement {

	private OWLOntology ontology;
	private OWLOntologyManager manager;
	private OWLReasoner reasoner;

	public OwlapiStatement(JopaStatement statement) {
		super(statement);
	}

	@Override
	public ResultSet executeStatement() {
		assert query != null;
		assert ontology != null;
		assert manager != null;
		assert reasoner != null;

		final OWLAPIv3OWL2Ontology ont = new OWLAPIv3OWL2Ontology(manager, ontology, reasoner);

		final QueryResult<OWLObject> res = OWL2QueryEngine.exec(query, ont);
		if (res == null) {
			throw new QueryExecutionException("Unable to execute query " + query);
		}
		return new OwlapiResultSet(res, jopaStatement);
	}

	public void setOntology(OWLOntology ontology) {
		this.ontology = ontology;
	}

	public void setOntologyManager(OWLOntologyManager manager) {
		this.manager = manager;
	}

	public void setReasoner(OWLReasoner reasoner) {
		this.reasoner = reasoner;
	}
}

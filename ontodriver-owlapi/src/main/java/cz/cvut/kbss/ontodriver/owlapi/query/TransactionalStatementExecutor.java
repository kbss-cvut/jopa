package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.exception.OwlapiDriverException;
import cz.cvut.kbss.ontodriver.owlapi.exception.ReasonerNotAvailableException;
import cz.cvut.kbss.ontodriver_new.Statement;
import cz.cvut.kbss.owl2query.engine.OWL2QueryEngine;
import cz.cvut.kbss.owl2query.model.QueryResult;
import cz.cvut.kbss.owl2query.model.owlapi.OWLAPIv3OWL2Ontology;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

public class TransactionalStatementExecutor implements StatementExecutor {

    private final OWLOntology ontology;
    private final OWLOntologyManager ontologyManager;
    private final OWLReasoner reasoner;

    public TransactionalStatementExecutor(OntologySnapshot snapshot) {
        this.ontology = snapshot.getOntology();
        this.ontologyManager = snapshot.getOntologyManager();
        this.reasoner = snapshot.getReasoner();
    }

    @Override
    public OwlapiResultSet executeQuery(String query, Statement statement) throws OwlapiDriverException {
        if (reasoner == null) {
            throw new ReasonerNotAvailableException("Cannot execute query without a reasoner.");
        }
        final OWLAPIv3OWL2Ontology ont = new OWLAPIv3OWL2Ontology(ontologyManager, ontology, reasoner);

        final QueryResult<OWLObject> res = OWL2QueryEngine.exec(query, ont);
        if (res == null) {
            throw new OwlapiDriverException("Unable to evaluate statement " + query);
        }
        return new OwlapiResultSet(res, statement);
    }

    @Override
    public void executeUpdate(String update) throws OwlapiDriverException {
        if (reasoner == null) {
            throw new ReasonerNotAvailableException("Cannot execute query without a reasoner.");
        }
        final OWLAPIv3OWL2Ontology ont = new OWLAPIv3OWL2Ontology(ontologyManager, ontology, reasoner);

        final QueryResult<OWLObject> res = OWL2QueryEngine.exec(update, ont);
        if (res == null) {
            throw new OwlapiDriverException("Unable to evaluate statement " + update);
        }
    }
}

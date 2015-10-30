package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.owlapi.connector.Connector;
import cz.cvut.kbss.ontodriver.owlapi.exception.OwlapiDriverException;
import cz.cvut.kbss.ontodriver.owlapi.exception.ReasonerNotAvailableException;
import cz.cvut.kbss.ontodriver_new.Statement;
import cz.cvut.kbss.owl2query.engine.OWL2QueryEngine;
import cz.cvut.kbss.owl2query.model.QueryResult;
import cz.cvut.kbss.owl2query.model.owlapi.OWLAPIv3OWL2Ontology;
import org.semanticweb.owlapi.model.OWLObject;

public class LiveOntologyStatementExecutor implements StatementExecutor {

    private final Connector connector;

    public LiveOntologyStatementExecutor(Connector connector) {
        this.connector = connector;
    }

    @Override
    public OwlapiResultSet executeQuery(final String query, Statement statement) throws OwlapiDriverException {
        final OwlapiResultSet resultSet = connector.executeRead(snapshot -> {
            if (snapshot.getReasoner() == null) {
                throw new ReasonerNotAvailableException("Cannot execute query without a reasoner.");
            }
            final OWLAPIv3OWL2Ontology ont = new OWLAPIv3OWL2Ontology(snapshot.getOntologyManager(),
                    snapshot.getOntology(), snapshot.getReasoner());

            final QueryResult<OWLObject> res = OWL2QueryEngine.exec(query, ont);

            return res != null ? new OwlapiResultSet(res, statement) : null;
        });
        if (resultSet == null) {
            throw new OwlapiDriverException("Unable to execute query " + query);
        }
        return resultSet;
    }

    @Override
    public void executeUpdate(final String update) throws OwlapiDriverException {
        connector.executeWrite(snapshot -> {
            if (snapshot.getReasoner() == null) {
                throw new ReasonerNotAvailableException("Cannot execute query without a reasoner.");
            }
            final OWLAPIv3OWL2Ontology ont = new OWLAPIv3OWL2Ontology(snapshot.getOntologyManager(),
                    snapshot.getOntology(), snapshot.getReasoner());

            OWL2QueryEngine.exec(update, ont);
        });
    }
}

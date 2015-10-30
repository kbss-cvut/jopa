package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.owlapi.connector.Connector;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver_new.Statement;

public class StatementExecutorFactory {

    private final OntologySnapshot transactionalSnapshot;
    private final Connector connector;

    public StatementExecutorFactory(OntologySnapshot transactionalSnapshot, Connector connector) {
        this.transactionalSnapshot = transactionalSnapshot;
        this.connector = connector;
    }

    public StatementExecutor getStatementExecutor(Statement.StatementOntology ontology) {
        assert ontology != null;

        switch (ontology) {
            case TRANSACTIONAL:
                return new TransactionalStatementExecutor(transactionalSnapshot);
            case CENTRAL:
                return new LiveOntologyStatementExecutor(connector);
            default:
                throw new IllegalArgumentException("Unsupported statement ontology type " + ontology);
        }
    }
}

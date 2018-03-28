package cz.cvut.kbss.ontodriver.jena.query;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.jena.connector.StatementExecutor;

public class JenaStatement implements Statement {

    private final StatementExecutor executor;

    public JenaStatement(StatementExecutor executor) {
        this.executor = executor;
    }

    @Override
    public ResultSet executeQuery(String sparql) throws OntoDriverException {
        return null;
    }

    @Override
    public void executeUpdate(String sparql) throws OntoDriverException {

    }

    @Override
    public void useOntology(StatementOntology ontology) {

    }

    @Override
    public StatementOntology getStatementOntology() {
        return null;
    }

    @Override
    public void close() throws Exception {

    }
}

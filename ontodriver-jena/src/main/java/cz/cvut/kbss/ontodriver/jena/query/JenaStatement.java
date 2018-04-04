package cz.cvut.kbss.ontodriver.jena.query;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.jena.connector.StatementExecutor;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.QueryParseException;

import java.util.Objects;

public class JenaStatement implements Statement {

    private final StatementExecutor executor;

    // We are not using the target ontology setting at the moment
    private StatementOntology targetOntology = StatementOntology.CENTRAL;
    private boolean open = true;

    private AbstractResultSet currentResultSet;

    public JenaStatement(StatementExecutor executor) {
        this.executor = executor;
    }

    @Override
    public ResultSet executeQuery(String sparql) throws JenaDriverException {
        ensureOpen();
        final Query query = parseQuery(Objects.requireNonNull(sparql));
        closeCurrentResultSet();
        final AbstractResultSet resultSet;
        if (query.isAskType()) {
            resultSet = executor.executeAskQuery(query);
        } else {
            resultSet = executor.executeSelectQuery(query);
        }
        resultSet.setStatement(this);
        this.currentResultSet = resultSet;
        return resultSet;
    }

    void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("Statement is closed.");
        }
    }

    private Query parseQuery(String sparql) throws JenaDriverException {
        try {
            return QueryFactory.create(sparql);
        } catch (QueryParseException e) {
            throw new JenaDriverException("Unable to parse query " + sparql, e);
        }
    }

    private void closeCurrentResultSet() throws JenaDriverException {
        if (currentResultSet != null) {
            currentResultSet.close();
            this.currentResultSet = null;
        }
    }

    @Override
    public void executeUpdate(String sparql) throws JenaDriverException {
        ensureOpen();
        Objects.requireNonNull(sparql);
        closeCurrentResultSet();
        executor.executeUpdate(sparql);
    }

    @Override
    public void useOntology(StatementOntology ontology) {
        ensureOpen();
        this.targetOntology = Objects.requireNonNull(ontology);
    }

    @Override
    public StatementOntology getStatementOntology() {
        return targetOntology;
    }

    @Override
    public void close() throws JenaDriverException {
        this.open = false;
        closeCurrentResultSet();
    }

    @Override
    public boolean isOpen() {
        return open;
    }
}

package cz.cvut.kbss.ontodriver.sesame.query;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.connector.StatementExecutor;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import org.openrdf.query.QueryEvaluationException;
import org.openrdf.query.TupleQueryResult;

import java.net.URI;
import java.util.Objects;

import static cz.cvut.kbss.ontodriver.util.ErrorUtils.npxMessage;

public class SesameStatement implements Statement {

    protected StatementOntology targetOntology = StatementOntology.TRANSACTIONAL;
    protected final StatementExecutor queryExecutor;
    protected ResultSet resultSet;

    private boolean open;

    public SesameStatement(StatementExecutor queryExecutor) {
        this.queryExecutor = queryExecutor;
        this.open = true;
    }

    @Override
    public ResultSet executeQuery(String sparql, URI... contexts) throws OntoDriverException {
        ensureOpen();
        validateQueryParams(sparql);
        determineResult(sparql);
        return resultSet;
    }

    private void determineResult(String sparql) throws SesameDriverException {
        if (isAskQuery(sparql)) {
            this.resultSet = new AskResultSet(queryExecutor.executeBooleanQuery(sparql), this);
        } else {
            final TupleQueryResult tqr = queryExecutor.executeSelectQuery(sparql);
            try {
                this.resultSet = new SelectResultSet(tqr, this);
            } catch (QueryEvaluationException e) {
                throw new SesameDriverException(e);
            }
        }
    }

    private boolean isAskQuery(String query) {
        return query.startsWith("ASK");
    }

    @Override
    public void executeUpdate(String sparql, URI... contexts) throws OntoDriverException {
        ensureOpen();
        validateQueryParams(sparql);
        queryExecutor.executeUpdate(sparql);
    }

    @Override
    public void useOntology(StatementOntology ontology) {
        this.targetOntology = ontology;
    }

    @Override
    public StatementOntology getStatementOntology() {
        return targetOntology;
    }

    private void validateQueryParams(String sparql) {
        Objects.requireNonNull(sparql, npxMessage("sparql"));
        if (sparql.isEmpty()) {
            throw new IllegalArgumentException("Query string cannot be empty.");
        }
    }

    public void close() throws Exception {
        if (!open) {
            return;
        }
        this.open = false;
        if (resultSet != null) {
            resultSet.close();
            this.resultSet = null;
        }
    }

    public void setUseTransactionalOntology() {
        ensureOpen();
        this.targetOntology = StatementOntology.TRANSACTIONAL;
    }

    public boolean useTransactionalOntology() {
        ensureOpen();
        return targetOntology == StatementOntology.TRANSACTIONAL;
    }

    public void setUseBackupOntology() {
        ensureOpen();
        this.targetOntology = StatementOntology.CENTRAL;
    }

    public boolean useBackupOntology() {
        ensureOpen();
        return targetOntology == StatementOntology.CENTRAL;
    }

    protected void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("This statement is closed.");
        }
    }
}

package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiConnection;
import cz.cvut.kbss.ontodriver_new.ResultSet;
import cz.cvut.kbss.ontodriver_new.Statement;

import java.net.URI;
import java.util.Objects;

public class OwlapiStatement implements Statement {

    private StatementOntology targetOntology;
    private boolean open;

    private final StatementExecutorFactory executorFactory;
    private final OwlapiConnection connection;

    public OwlapiStatement(StatementExecutorFactory executorFactory, OwlapiConnection connection) {
        this.executorFactory = executorFactory;
        this.connection = connection;
        this.open = true;
    }

    private void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("The statement is closed.");
        }
    }

    @Override
    public ResultSet executeQuery(String sparql, URI... contexts) throws OntoDriverException {
        ensureOpen();
        Objects.requireNonNull(sparql);
        return getExecutor().executeQuery(sparql, this);
    }

    private StatementExecutor getExecutor() {
        return executorFactory.getStatementExecutor(targetOntology);
    }

    @Override
    public void executeUpdate(String sparql, URI... contexts) throws OntoDriverException {
        ensureOpen();
        Objects.requireNonNull(sparql);
        getExecutor().executeUpdate(sparql);
        connection.commitIfAuto();
    }

    @Override
    public void setUseTransactionalOntology() {
        this.targetOntology = StatementOntology.TRANSACTIONAL;
    }

    @Override
    public boolean useTransactionalOntology() {
        return targetOntology == StatementOntology.TRANSACTIONAL;
    }

    @Override
    public void setUseBackupOntology() {
        this.targetOntology = StatementOntology.CENTRAL;
    }

    @Override
    public boolean useBackupOntology() {
        return targetOntology == StatementOntology.CENTRAL;
    }

    @Override
    public void useOntology(StatementOntology ontology) {
        this.targetOntology = ontology;
    }

    @Override
    public StatementOntology getStatementOntology() {
        return targetOntology;
    }

    @Override
    public void close() throws Exception {
        this.open = false;
    }
}

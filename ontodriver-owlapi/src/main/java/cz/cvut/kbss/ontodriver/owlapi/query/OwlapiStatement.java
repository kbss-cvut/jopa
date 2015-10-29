package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver_new.ResultSet;
import cz.cvut.kbss.ontodriver_new.Statement;

import java.net.URI;

public class OwlapiStatement implements Statement {

    private StatementOntology targetOntology;
    private boolean open;

    private void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("The statement is closed.");
        }
    }

    @Override
    public ResultSet executeQuery(String sparql, URI... contexts) throws OntoDriverException {
        return null;
    }

    @Override
    public void executeUpdate(String sparql, URI... contexts) throws OntoDriverException {

    }

    @Override
    public void setUseTransactionalOntology() {
        ensureOpen();
        this.targetOntology = StatementOntology.TRANSACTIONAL;
    }

    @Override
    public boolean useTransactionalOntology() {
        ensureOpen();
        return  targetOntology == StatementOntology.TRANSACTIONAL;
    }

    @Override
    public void setUseBackupOntology() {
        ensureOpen();
        this.targetOntology = StatementOntology.CENTRAL;
    }

    @Override
    public boolean useBackupOntology() {
        ensureOpen();
        return targetOntology == StatementOntology.CENTRAL;
    }

    @Override
    public void useOntology(StatementOntology ontology) {
        ensureOpen();
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

package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver_new.PreparedStatement;

import java.net.URI;

public class OwlapiPreparedStatement implements PreparedStatement {

    @Override
    public ResultSet executeQuery() throws OntoDriverException {
        return null;
    }

    @Override
    public void executeUpdate() throws OntoDriverException {

    }

    @Override
    public void setObject(String binding, Object value) throws OntoDriverException {

    }

    @Override
    public void clearParameters() throws OntoDriverException {

    }

    @Override
    public cz.cvut.kbss.ontodriver_new.ResultSet executeQuery(String sparql, URI... contexts)
            throws OntoDriverException {
        return null;
    }

    @Override
    public void executeUpdate(String sparql, URI... contexts) throws OntoDriverException {

    }

    @Override
    public void useOntology(StatementOntology ontology) {

    }

    @Override
    public StatementOntology getStatementOntology() {
        return null;
    }

    @Override
    public void setUseTransactionalOntology() {

    }

    @Override
    public boolean useTransactionalOntology() {
        return false;
    }

    @Override
    public void setUseBackupOntology() {

    }

    @Override
    public boolean useBackupOntology() {
        return false;
    }

    @Override
    public void close() throws Exception {

    }
}

package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver_new.ResultSet;
import cz.cvut.kbss.ontodriver_new.Statement;

import java.net.URI;

public class OwlapiStatement implements Statement {


    @Override
    public ResultSet executeQuery(String sparql, URI... contexts) throws OntoDriverException {
        return null;
    }

    @Override
    public void executeUpdate(String sparql, URI... contexts) throws OntoDriverException {

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

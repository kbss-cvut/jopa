package cz.cvut.kbss.jopa.accessors;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.PreparedStatement;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver_new.*;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Axiom;

import java.net.URI;
import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * Created by ledvima1 on 12.12.14.
 */
public class DataSourceStub implements DataSource {

    private boolean open = true;

    @Override
    public Connection getConnection() throws OntoDriverException {
        return new ConnectionStub();
    }

    @Override
    public void setStorageProperties(OntologyStorageProperties storageProperties) throws OntoDriverException {
        // Do nothing
    }

    @Override
    public void setProperties(Map<String, String> properties) throws OntoDriverException {
        // Do nothing
    }

    @Override
    public void close() throws OntoDriverException {
        open = false;
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    static final class ConnectionStub implements Connection {

        @Override
        public boolean isOpen() {
            return false;
        }

        @Override
        public void commit() throws OntoDriverException {

        }

        @Override
        public void rollback() throws OntoDriverException {

        }

        @Override
        public void setAutoCommit(boolean autoCommit) {

        }

        @Override
        public boolean isAutoCommit() {
            return false;
        }

        @Override
        public Statement createStatement() throws OntoDriverException {
            return null;
        }

        @Override
        public PreparedStatement prepareStatement(String sparql) throws OntoDriverException {
            return null;
        }

        @Override
        public boolean isConsistent(URI context) throws OntoDriverException {
            return false;
        }

        @Override
        public List<URI> getContexts() throws OntoDriverException {
            return null;
        }

        @Override
        public boolean contains(Axiom<?> axiom, URI context) throws OntoDriverException {
            return false;
        }

        @Override
        public Collection<Axiom<?>> find(AxiomDescriptor descriptor) throws OntoDriverException {
            return null;
        }

        @Override
        public void persist(AxiomValueDescriptor descriptor) throws OntoDriverException {

        }

        @Override
        public URI generateIdentifier(URI classUri) throws OntoDriverException {
            return null;
        }

        @Override
        public void update(AxiomValueDescriptor descriptor) throws OntoDriverException {

        }

        @Override
        public void remove(AxiomDescriptor descriptor) throws OntoDriverException {

        }

        @Override
        public Lists lists() {
            return null;
        }

        @Override
        public Types types() {
            return null;
        }

        @Override
        public Properties properties() {
            return null;
        }

        @Override
        public void close() throws Exception {

        }
    }
}

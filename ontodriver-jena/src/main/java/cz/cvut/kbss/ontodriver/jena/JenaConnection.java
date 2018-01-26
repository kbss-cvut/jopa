package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.*;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;

import java.net.URI;
import java.util.Collection;
import java.util.List;

class JenaConnection implements Connection {

    private boolean open;
    private boolean autoCommit;

    public JenaConnection() {
        this.open = true;
    }

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
        ensureOpen();
        this.autoCommit = autoCommit;
    }

    @Override
    public boolean isAutoCommit() {
        ensureOpen();
        return autoCommit;
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
    public <T> T unwrap(Class<T> cls) throws OntoDriverException {
        return null;
    }

    @Override
    public void close() {
        if (!open) {
            return;
        }
        this.open = false;
    }

    private void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("This connection is closed.");
        }
    }
}

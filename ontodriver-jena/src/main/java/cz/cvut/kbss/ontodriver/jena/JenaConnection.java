package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.*;
import cz.cvut.kbss.ontodriver.Properties;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import cz.cvut.kbss.ontodriver.jena.util.ConnectionListener;
import cz.cvut.kbss.ontodriver.model.Axiom;

import java.net.URI;
import java.util.*;

public class JenaConnection implements Connection {

    private boolean open;
    private boolean autoCommit;

    private final Set<ConnectionListener> listeners = new HashSet<>(4);

    private final JenaAdapter adapter;

    JenaConnection(JenaAdapter adapter) {
        this.adapter = adapter;
        this.open = true;
    }

    void registerListener(ConnectionListener listener) {
        ensureOpen();
        listeners.add(listener);
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    @Override
    public void commit() throws JenaDriverException {
        ensureOpen();
        if (!autoCommit) {
            adapter.commit();
        }
    }

    @Override
    public void rollback() {
        ensureOpen();
        if (!autoCommit) {
            adapter.rollback();
        }
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

    private void commitIfAuto() throws JenaDriverException {
        if (autoCommit) {
            adapter.commit();
        }
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
    public void persist(AxiomValueDescriptor descriptor) throws JenaDriverException {
        ensureOpen();
        Objects.requireNonNull(descriptor);
        try {
            adapter.persist(descriptor);
            commitIfAuto();
        } catch (RuntimeException e) {
            throw new JenaDriverException(e);
        }
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
    public void close() throws JenaDriverException {
        if (!open) {
            return;
        }
        adapter.close();
        listeners.forEach(listener -> listener.connectionClosed(this));
        this.open = false;
    }

    private void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("This connection is closed.");
        }
    }
}

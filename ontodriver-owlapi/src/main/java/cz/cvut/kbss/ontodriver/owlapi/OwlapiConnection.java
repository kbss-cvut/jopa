package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.PreparedStatement;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver_new.Connection;
import cz.cvut.kbss.ontodriver_new.Lists;
import cz.cvut.kbss.ontodriver_new.Properties;
import cz.cvut.kbss.ontodriver_new.Types;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Axiom;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Created by ledvima1 on 26.2.15.
 */
public class OwlapiConnection implements Connection {

    private boolean open;
    private boolean autoCommit;

    private final OwlapiAdapter adapter;

    private final List<ConnectionListener> listeners = new ArrayList<>(4);

    public OwlapiConnection(OwlapiAdapter adapter) {
        this.adapter = adapter;
        this.open = true;
        this.autoCommit = false;
    }

    void addListener(ConnectionListener listener) {
        assert listener != null;
        listeners.add(listener);
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    @Override
    public void commit() throws OntoDriverException {
        ensureOpen();
        adapter.commit();
    }

    private void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("Connection is closed.");
        }
    }

    @Override
    public void rollback() throws OntoDriverException {
        ensureOpen();
        adapter.rollback();
    }

    @Override
    public void setAutoCommit(boolean autoCommit) {
        this.autoCommit = autoCommit;
    }

    @Override
    public boolean isAutoCommit() {
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
    public void close() throws Exception {
        if (!open) {
            return;
        }
        listeners.stream().forEach(listener -> listener.connectionClosed(this));
        this.open = false;
    }
}

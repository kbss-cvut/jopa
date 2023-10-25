/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.*;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.IdentifierGenerationException;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.query.Rdf4jPreparedStatement;
import cz.cvut.kbss.ontodriver.rdf4j.query.Rdf4jStatement;

import java.net.URI;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import static cz.cvut.kbss.ontodriver.util.ErrorUtils.getNPXMessageSupplier;


class Rdf4jConnection implements Connection {

    private final Rdf4jAdapter adapter;
    private boolean open;
    private boolean autoCommit;

    private Lists lists;
    private Types types;
    private Properties properties;

    private ConnectionListener<Rdf4jConnection> listener;

    Rdf4jConnection(Rdf4jAdapter adapter) {
        assert adapter != null;
        this.adapter = adapter;
        this.open = true;
    }

    void setLists(Rdf4jLists lists) {
        this.lists = lists;
    }

    void setTypes(Rdf4jTypes types) {
        this.types = types;
    }

    public void setProperties(Properties properties) {
        this.properties = properties;
    }

    void setListener(ConnectionListener<Rdf4jConnection> listener) {
        ensureOpen();
        assert listener != null;
        this.listener = listener;
    }

    void removeListener() {
        this.listener = null;
    }

    @Override
    public void close() throws Exception {
        if (!open) {
            return;
        }
        try {
            adapter.close();
            if (listener != null) {
                listener.connectionClosed(this);
            }
        } finally {
            this.open = false;
        }
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    @Override
    public void commit() throws OntoDriverException {
        ensureOpen();
        if (autoCommit) {
            return;
        }
        adapter.commit();
    }

    @Override
    public void rollback() throws OntoDriverException {
        ensureOpen();
        if (autoCommit) {
            return;
        }
        adapter.rollback();
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
    public Statement createStatement() {
        ensureOpen();
        return new Rdf4jStatement(adapter.getQueryExecutor());
    }

    @Override
    public PreparedStatement prepareStatement(String sparql) {
        ensureOpen();
        Objects.requireNonNull(sparql);
        if (sparql.isEmpty()) {
            throw new IllegalArgumentException("The value for prepared statement cannot be empty.");
        }
        return new Rdf4jPreparedStatement(adapter.getQueryExecutor(), sparql);
    }

    @Override
    public boolean isConsistent(URI context) {
        ensureOpen();
        return adapter.isConsistent(context);
    }

    @Override
    public List<URI> getContexts() throws OntoDriverException {
        ensureOpen();
        return adapter.getContexts();
    }

    @Override
    public URI generateIdentifier(URI classUri) throws OntoDriverException {
        ensureOpen();
        Objects.requireNonNull(classUri);
        try {
            return adapter.generateIdentifier(classUri);
        } catch (IdentifierGenerationException e) {
            throw new Rdf4jDriverException(e);
        }
    }

    @Override
    public boolean contains(Axiom<?> axiom, Set<URI> contexts) throws OntoDriverException {
        ensureOpen();
        Objects.requireNonNull(axiom, getNPXMessageSupplier("axiom"));
        Objects.requireNonNull(contexts);
        return adapter.contains(axiom, contexts);
    }

    @Override
    public boolean isInferred(Axiom<?> axiom, Set<URI> contexts) throws OntoDriverException {
        ensureOpen();
        Objects.requireNonNull(axiom, getNPXMessageSupplier("axiom"));
        Objects.requireNonNull(contexts);
        return adapter.isInferred(axiom, contexts);
    }

    @Override
    public Collection<Axiom<?>> find(AxiomDescriptor descriptor) throws OntoDriverException {
        ensureOpen();
        Objects.requireNonNull(descriptor);
        try {
            return adapter.find(descriptor);
        } catch (RuntimeException e) {
            throw new Rdf4jDriverException(e);
        }
    }

    @Override
    public void persist(AxiomValueDescriptor descriptor) throws OntoDriverException {
        ensureOpen();
        Objects.requireNonNull(descriptor);
        try {
            adapter.persist(descriptor);
            commitIfAuto();
        } catch (RuntimeException e) {
            throw new Rdf4jDriverException(e);
        }
    }

    @Override
    public void update(AxiomValueDescriptor descriptor) throws OntoDriverException {
        ensureOpen();
        Objects.requireNonNull(descriptor);
        try {
            adapter.update(descriptor);
            commitIfAuto();
        } catch (RuntimeException e) {
            throw new Rdf4jDriverException(e);
        }
    }

    @Override
    public void remove(AxiomDescriptor descriptor) throws OntoDriverException {
        ensureOpen();
        Objects.requireNonNull(descriptor);
        try {
            adapter.remove(descriptor);
            commitIfAuto();
        } catch (RuntimeException e) {
            throw new Rdf4jDriverException(e);
        }
    }

    @Override
    public Lists lists() {
        ensureOpen();
        assert lists != null;
        return lists;
    }

    @Override
    public Types types() {
        ensureOpen();
        assert types != null;
        return types;
    }

    @Override
    public Properties properties() {
        ensureOpen();
        assert properties != null;
        return properties;
    }

    void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("This connection is closed.");
        }
    }

    void commitIfAuto() throws Rdf4jDriverException {
        if (autoCommit) {
            adapter.commit();
        }
    }

    @Override
    public <T> T unwrap(Class<T> cls) throws OntoDriverException {
        if (cls.isAssignableFrom(this.getClass())) {
            return cls.cast(this);
        }
        return adapter.unwrap(cls);
    }
}

/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.Containers;
import cz.cvut.kbss.ontodriver.PreparedStatement;
import cz.cvut.kbss.ontodriver.RepositoryMetadata;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import cz.cvut.kbss.ontodriver.jena.list.JenaLists;
import cz.cvut.kbss.ontodriver.jena.query.JenaStatement;
import cz.cvut.kbss.ontodriver.jena.util.ConnectionListener;
import cz.cvut.kbss.ontodriver.model.Axiom;

import java.net.URI;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Set;

public class JenaConnection implements Connection {

    private boolean open;
    private boolean autoCommit;

    private ConnectionListener listener;

    private final JenaAdapter adapter;

    JenaConnection(JenaAdapter adapter) {
        this.adapter = adapter;
        this.open = true;
    }

    void registerListener(ConnectionListener listener) {
        ensureOpen();
        this.listener = listener;
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

    @Override
    public void setReadOnly(boolean readOnly) {
        ensureOpen();
        adapter.setReadOnly(readOnly);
    }

    @Override
    public boolean isReadOnly() {
        ensureOpen();
        return adapter.isReadOnly();
    }

    private void commitIfAuto() throws JenaDriverException {
        if (autoCommit) {
            adapter.commit();
        }
    }

    @Override
    public JenaStatement createStatement() {
        ensureOpen();
        return adapter.createStatement();
    }

    @Override
    public PreparedStatement prepareStatement(String sparql) {
        ensureOpen();
        return adapter.prepareStatement(Objects.requireNonNull(sparql));
    }

    @Override
    public boolean isConsistent(URI context) {
        ensureOpen();
        return adapter.isConsistent(context);
    }

    @Override
    public List<URI> getContexts() {
        ensureOpen();
        return adapter.getContext();
    }

    @Override
    public boolean contains(Axiom<?> axiom, Set<URI> contexts) throws JenaDriverException {
        ensureOpen();
        Objects.requireNonNull(axiom);
        Objects.requireNonNull(contexts);
        try {
            return adapter.contains(axiom, contexts);
        } catch (RuntimeException e) {
            throw new JenaDriverException(e);
        }
    }

    @Override
    public boolean isInferred(Axiom<?> axiom, Set<URI> contexts) throws JenaDriverException {
        ensureOpen();
        Objects.requireNonNull(axiom);
        Objects.requireNonNull(contexts);
        try {
            return adapter.isInferred(axiom, contexts);
        } catch (RuntimeException e) {
            throw new JenaDriverException(e);
        }
    }

    @Override
    public Collection<Axiom<?>> find(AxiomDescriptor descriptor) throws JenaDriverException {
        ensureOpen();
        Objects.requireNonNull(descriptor);
        try {
            return adapter.find(descriptor);
        } catch (RuntimeException e) {
            throw new JenaDriverException(e);
        }
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
    public URI generateIdentifier(URI classUri) {
        ensureOpen();
        Objects.requireNonNull(classUri);
        return adapter.generateIdentifier(classUri);
    }

    @Override
    public void update(AxiomValueDescriptor descriptor) throws OntoDriverException {
        ensureOpen();
        Objects.requireNonNull(descriptor);
        adapter.update(descriptor);
        commitIfAuto();
    }

    @Override
    public void remove(AxiomDescriptor descriptor) throws OntoDriverException {
        ensureOpen();
        Objects.requireNonNull(descriptor);
        try {
            adapter.remove(descriptor);
            commitIfAuto();
        } catch (RuntimeException e) {
            throw new JenaDriverException(e);
        }
    }

    @Override
    public JenaLists lists() {
        ensureOpen();
        return new JenaLists(adapter, this::ensureOpen, this::commitIfAuto);
    }

    @Override
    public JenaTypes types() {
        ensureOpen();
        return new JenaTypes(adapter, this::ensureOpen, this::commitIfAuto);
    }

    @Override
    public JenaProperties properties() {
        ensureOpen();
        return new JenaProperties(adapter, this::ensureOpen, this::commitIfAuto);
    }

    @Override
    public Containers containers() {
        ensureOpen();
        return new JenaContainers(adapter, this::ensureOpen, this::commitIfAuto);
    }

    @Override
    public <T> T unwrap(Class<T> cls) throws OntoDriverException {
        ensureOpen();
        Objects.requireNonNull(cls);
        if (cls.isAssignableFrom(getClass())) {
            return cls.cast(this);
        }
        return adapter.unwrap(cls);
    }

    @Override
    public void close() throws JenaDriverException {
        if (!open) {
            return;
        }
        adapter.close();
        if (listener != null) {
            listener.connectionClosed(this);
        }
        this.open = false;
    }

    private void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("This connection is closed.");
        }
    }

    @Override
    public RepositoryMetadata getRepositoryMetadata() {
        ensureOpen();
        return new JenaRepositoryMetadata();
    }
}

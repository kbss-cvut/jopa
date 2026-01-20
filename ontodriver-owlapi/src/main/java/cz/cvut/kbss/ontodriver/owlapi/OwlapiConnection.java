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
package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.Containers;
import cz.cvut.kbss.ontodriver.Lists;
import cz.cvut.kbss.ontodriver.PreparedStatement;
import cz.cvut.kbss.ontodriver.Properties;
import cz.cvut.kbss.ontodriver.RepositoryMetadata;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.Types;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.owlapi.exception.OwlapiDriverException;
import cz.cvut.kbss.ontodriver.owlapi.list.OwlapiLists;

import java.net.URI;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Set;

/**
 * Default implementation of the {@link Connection} interface for OWLAPI driver.
 */
public class OwlapiConnection implements Connection {

    private boolean open;
    private boolean autoCommit;

    private final OwlapiAdapter adapter;

    private OwlapiTypes types;
    private OwlapiProperties properties;
    private OwlapiLists lists;
    private OwlapiContainers containers;

    private ConnectionListener listener;

    OwlapiConnection(OwlapiAdapter adapter) {
        this.adapter = adapter;
        this.open = true;
        this.autoCommit = false;
    }

    void setListener(ConnectionListener listener) {
        ensureOpen();
        assert listener != null;
        this.listener = listener;
    }

    void removeListener() {
        this.listener = null;
    }

    void setTypes(OwlapiTypes types) {
        this.types = types;
    }

    void setProperties(OwlapiProperties properties) {
        this.properties = properties;
    }

    void setLists(OwlapiLists lists) {
        this.lists = lists;
    }

    void setContainers(OwlapiContainers containers) {
        this.containers = containers;
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    @Override
    public void commit() {
        ensureOpen();
        adapter.commit();
    }

    public void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("Connection is closed.");
        }
    }

    @Override
    public void rollback() {
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
    public void setReadOnly(boolean readOnly) {
        ensureOpen();
        // Do nothing, unsupported
    }

    @Override
    public boolean isReadOnly() {
        ensureOpen();
        return false;
    }

    @Override
    public Statement createStatement() {
        ensureOpen();
        return adapter.createStatement(this);
    }

    @Override
    public PreparedStatement prepareStatement(String sparql) {
        ensureOpen();
        return adapter.prepareStatement(sparql, this);
    }

    @Override
    public boolean isConsistent(URI context) {
        ensureOpen();
        return adapter.isConsistent(context);
    }

    @Override
    public List<URI> getContexts() {
        ensureOpen();
        return adapter.getContexts();
    }

    @Override
    public boolean contains(Axiom<?> axiom, Set<URI> contexts) {
        ensureOpen();
        Objects.requireNonNull(axiom);
        return adapter.containsAxiom(axiom, contexts);
    }

    @Override
    public boolean isInferred(Axiom<?> axiom, Set<URI> contexts) {
        ensureOpen();
        Objects.requireNonNull(axiom);
        return adapter.isInferred(axiom, contexts);
    }

    @Override
    public Collection<Axiom<?>> find(AxiomDescriptor descriptor) throws OntoDriverException {
        ensureOpen();
        Objects.requireNonNull(descriptor);
        try {
            return adapter.find(descriptor);
        } catch (RuntimeException e) {
            throw new OwlapiDriverException(e);
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
            throw new OwlapiDriverException(e);
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
        try {
            adapter.update(descriptor);
            commitIfAuto();
        } catch (RuntimeException e) {
            throw new OwlapiDriverException(e);
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
            throw new OwlapiDriverException(e);
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

    @Override
    public Containers containers() {
        ensureOpen();
        assert containers != null;
        return containers;
    }

    @Override
    public void close() {
        if (!open) {
            return;
        }
        if (listener != null) {
            listener.connectionClosed(this);
        }
        this.open = false;
    }

    public void commitIfAuto() {
        if (autoCommit) {
            adapter.commit();
        }
    }

    @Override
    public RepositoryMetadata getRepositoryMetadata() {
        ensureOpen();
        return new OwlapiRepositoryMetadata();
    }

    @Override
    public <T> T unwrap(Class<T> cls) throws OntoDriverException {
        if (cls.isAssignableFrom(this.getClass())) {
            return cls.cast(this);
        }
        return adapter.unwrap(cls);
    }
}

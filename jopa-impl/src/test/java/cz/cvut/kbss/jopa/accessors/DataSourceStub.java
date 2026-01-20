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
package cz.cvut.kbss.jopa.accessors;

import cz.cvut.kbss.ontodriver.*;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;

import java.net.URI;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class DataSourceStub implements DataSource {

    private boolean open = true;

    private boolean exceptionOnGetConnection;
    private boolean exceptionOnClose;

    void throwExceptionOnGetConnection() {
        this.exceptionOnGetConnection = true;
    }

    @Override
    public Connection getConnection() throws OntoDriverException {
        if (exceptionOnGetConnection) {
            throw new OntoDriverException("Exception when getting connection.");
        }
        return new ConnectionStub();
    }

    @Override
    public void setStorageProperties(OntologyStorageProperties storageProperties) {
        // Do nothing
    }

    @Override
    public void setProperties(Map<String, String> properties) {
        // Do nothing
    }

    void throwExceptionOnClose() {
        this.exceptionOnClose = true;
    }

    @Override
    public void close() throws OntoDriverException {
        if (exceptionOnClose) {
            throw new OntoDriverException("Exception when closing the data source.");
        }
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
        public void commit() {}

        @Override
        public void rollback() {}

        @Override
        public void setAutoCommit(boolean autoCommit) {}

        @Override
        public boolean isAutoCommit() {
            return false;
        }

        @Override
        public void setReadOnly(boolean readOnly) {}

        @Override
        public boolean isReadOnly() {
            return false;
        }

        @Override
        public Statement createStatement() {
            return null;
        }

        @Override
        public PreparedStatement prepareStatement(String sparql) {
            return null;
        }

        @Override
        public boolean isConsistent(URI context) {
            return false;
        }

        @Override
        public List<URI> getContexts() {
            return null;
        }

        @Override
        public boolean contains(Axiom<?> axiom, Set<URI> context) {
            return false;
        }

        @Override
        public boolean isInferred(Axiom<?> axiom, Set<URI> contexts) {
            return true;
        }

        @Override
        public Collection<Axiom<?>> find(AxiomDescriptor descriptor) {
            return null;
        }

        @Override
        public void persist(AxiomValueDescriptor descriptor) {}

        @Override
        public URI generateIdentifier(URI classUri) {
            return null;
        }

        @Override
        public void update(AxiomValueDescriptor descriptor) {}

        @Override
        public void remove(AxiomDescriptor descriptor) {}

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
        public Containers containers() {
            return null;
        }

        @Override
        public void close() {}

        @Override
        public RepositoryMetadata getRepositoryMetadata() {
            throw new UnsupportedOperationException();
        }

        @Override
        public <T> T unwrap(Class<T> cls) {
            return null;
        }
    }
}

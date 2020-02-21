/**
 * Copyright (C) 2020 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.accessors;

import cz.cvut.kbss.ontodriver.*;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Axiom;

import java.net.URI;
import java.util.Collection;
import java.util.List;
import java.util.Map;

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
    public void setStorageProperties(OntologyStorageProperties storageProperties) throws OntoDriverException {
        // Do nothing
    }

    @Override
    public void setProperties(Map<String, String> properties) throws OntoDriverException {
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

        @Override
        public <T> T unwrap(Class<T> cls) throws OntoDriverException {
            return null;
        }
    }
}

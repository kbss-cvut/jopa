/**
 * Copyright (C) 2020 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.sesame.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.config.Constants;
import cz.cvut.kbss.ontodriver.sesame.config.SesameConfigParam;
import cz.cvut.kbss.ontodriver.sesame.config.SesameOntoDriverProperties;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import org.eclipse.rdf4j.common.iteration.Iterations;
import org.eclipse.rdf4j.model.*;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;
import org.eclipse.rdf4j.repository.RepositoryResult;
import org.eclipse.rdf4j.repository.manager.RepositoryManager;
import org.eclipse.rdf4j.repository.sail.SailRepository;
import org.eclipse.rdf4j.sail.Sail;
import org.eclipse.rdf4j.sail.helpers.SailWrapper;
import org.eclipse.rdf4j.sail.memory.MemoryStore;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collection;
import java.util.List;
import java.util.Objects;

class StorageConnector extends AbstractConnector {

    private static final Logger LOG = LoggerFactory.getLogger(StorageConnector.class);

    private final DriverConfiguration configuration;

    private final int maxReconnectAttempts;

    private Repository repository;
    private RepositoryManager manager;
    private RepositoryConnection connection;

    StorageConnector(DriverConfiguration configuration) throws SesameDriverException {
        assert configuration != null;

        this.configuration = configuration;
        try {
            this.maxReconnectAttempts = configuration.isSet(SesameConfigParam.RECONNECT_ATTEMPTS) ? Integer.parseInt(
                    configuration.getProperty(SesameConfigParam.RECONNECT_ATTEMPTS)) :
                                        Constants.DEFAULT_RECONNECT_ATTEMPTS_COUNT;
            if (maxReconnectAttempts < 0) {
                throw invalidReconnectAttemptsConfig();
            }
        } catch (NumberFormatException e) {
            throw invalidReconnectAttemptsConfig();
        }
        initialize();
        this.open = true;
    }

    private SesameDriverException invalidReconnectAttemptsConfig() {
        return new SesameDriverException(
                "Invalid value of configuration parameter " + SesameOntoDriverProperties.SESAME_RECONNECT_ATTEMPTS +
                        ". Must be a non-negative integer.");
    }

    private void initialize() throws SesameDriverException {
        final RepositoryConnectorInitializer initializer = new RepositoryConnectorInitializer(configuration,
                maxReconnectAttempts);
        initializer.initializeRepository();
        this.manager = initializer.getManager();
        this.repository = initializer.getRepository();
    }

    @Override
    public void close() throws SesameDriverException {
        if (!open) {
            return;
        }
        LOG.debug("Closing connector to repository {}.", configuration.getStorageProperties().getPhysicalURI());
        try {
            repository.shutDown();
            if (manager != null) {
                manager.shutDown();
            }
        } catch (RepositoryException e) {
            throw new SesameDriverException("Exception caught when closing Sesame repository connection.", e);
        } finally {
            this.open = false;
        }
    }

    @Override
    public TupleQueryResult executeSelectQuery(String query) throws SesameDriverException {
        final RepositoryConnection conn = acquireConnection();
        return new ConnectionStatementExecutor(conn).executeSelectQuery(query);
        // The connection is released by the result set once it is closed
    }

    RepositoryConnection acquireConnection() throws SesameDriverException {
        // Workaround for local native storage being reset when multiple drivers access it
        if (!repository.isInitialized()) {
            initialize();
        }
        try {
            LOG.trace("Acquiring repository connection.");
            return repository.getConnection();
        } catch (RepositoryException e) {
            throw new SesameDriverException(e);
        }
    }

    void releaseConnection(RepositoryConnection conn) throws SesameDriverException {
        try {
            if (conn != null) {
                LOG.trace("Releasing repository connection.");
                conn.close();
            }
        } catch (RepositoryException e) {
            throw new SesameDriverException(e);
        }
    }

    @Override
    public boolean executeBooleanQuery(String query) throws SesameDriverException {
        try (final RepositoryConnection conn = acquireConnection()) {
            return new ConnectionStatementExecutor(conn).executeBooleanQuery(query);
        }
    }

    @Override
    public void executeUpdate(String query) throws SesameDriverException {
        try (final RepositoryConnection conn = acquireConnection()) {
            new ConnectionStatementExecutor(conn).executeUpdate(query);
        }
    }

    @Override
    public List<Resource> getContexts() throws SesameDriverException {
        try (final RepositoryConnection conn = acquireConnection()) {
            final RepositoryResult<Resource> res = conn.getContextIDs();
            return Iterations.asList(res);
        } catch (RepositoryException e) {
            throw new SesameDriverException(e);
        }
    }

    @Override
    public ValueFactory getValueFactory() {
        return repository.getValueFactory();
    }

    @Override
    public void begin() throws SesameDriverException {
        super.begin();
        this.connection = acquireConnection();
        try {
            connection.begin();
        } catch (RepositoryException e) {
            transaction.rollback();
            throw new SesameDriverException(e);
        }
    }

    @Override
    public void commit() throws SesameDriverException {
        assert connection != null;

        transaction.commit();
        try {
            connection.commit();
            connection.close();
            this.connection = null;
            transaction.afterCommit();
        } catch (RepositoryException e) {
            transaction.rollback();
            throw new SesameDriverException(e);
        }
    }

    @Override
    public void rollback() throws SesameDriverException {
        assert connection != null;
        transaction.rollback();
        try {
            connection.rollback();
            connection.close();
            this.connection = null;
        } catch (RepositoryException e) {
            throw new SesameDriverException(e);
        } finally {
            transaction.afterRollback();
        }
    }

    @Override
    public void addStatements(Collection<Statement> statements) throws SesameDriverException {
        verifyTransactionActive();
        assert connection != null;

        try {
            connection.add(statements);
        } catch (RepositoryException e) {
            throw new SesameDriverException(e);
        }
    }

    @Override
    public void removeStatements(Collection<Statement> statements) throws SesameDriverException {
        verifyTransactionActive();
        assert connection != null;

        try {
            connection.remove(statements);
        } catch (RepositoryException e) {
            throw new SesameDriverException(e);
        }
    }

    @Override
    public Collection<Statement> findStatements(Resource subject, IRI property, Value value, boolean includeInferred)
            throws SesameDriverException {
        return findStatements(subject, property, value, includeInferred, null);
    }

    @Override
    public Collection<Statement> findStatements(Resource subject, org.eclipse.rdf4j.model.IRI property,
                                                Value value, boolean includeInferred, IRI context)
            throws SesameDriverException {
        try (final RepositoryConnection conn = acquireConnection()) {
            final RepositoryResult<Statement> m;
            if (context != null) {
                m = conn.getStatements(subject, property, null, includeInferred, context);
            } else {
                m = conn.getStatements(subject, property, null, includeInferred);
            }
            return Iterations.asList(m);
        } catch (RepositoryException e) {
            throw new SesameDriverException(e);
        }
    }

    @Override
    public boolean containsStatement(Resource subject, IRI property, Value value, boolean includeInferred)
            throws SesameDriverException {
        return containsStatement(subject, property, value, includeInferred, null);
    }

    @Override
    public boolean containsStatement(Resource subject, IRI property, Value value, boolean includeInferred, IRI context)
            throws SesameDriverException {
        try (final RepositoryConnection conn = acquireConnection()) {
            if (context != null) {
                return conn.hasStatement(subject, property, null, includeInferred, context);
            } else {
                return conn.hasStatement(subject, property, null, includeInferred);
            }
        } catch (RepositoryException e) {
            throw new SesameDriverException(e);
        }
    }

    @Override
    public <T> T unwrap(Class<T> cls) throws OntoDriverException {
        if (cls.isAssignableFrom(getClass())) {
            return cls.cast(this);
        }
        if (cls.isAssignableFrom(repository.getClass())) {
            return cls.cast(repository);
        }
        throw new SesameDriverException("No instance of class " + cls + " found.");
    }

    /**
     * Replaces the currently open repository with the specified one.
     * <p>
     * Note that this functionality is only supported for in-memory stores.
     *
     * @param newRepository The new repository to set
     */
    public void setRepository(Repository newRepository) {
        Objects.requireNonNull(newRepository);
        if (!isInMemoryRepository(repository)) {
            throw new UnsupportedOperationException("Cannot replace repository which is not in-memory.");
        }
        if (transaction.isActive()) {
            throw new IllegalStateException("Cannot replace repository in transaction.");
        }
        repository.shutDown();
        assert newRepository.isInitialized();
        this.repository = newRepository;
        // Since in-memory repositories are not registered in RepositoryManager, we shouldn't need to deal with it
    }

    private static boolean isInMemoryRepository(Repository repo) {
        if (!(repo instanceof SailRepository)) {
            return false;
        }
        final Sail sail = ((SailRepository) repo).getSail();
        return sail instanceof SailWrapper ? ((SailWrapper) sail).getBaseSail() instanceof MemoryStore :
               sail instanceof MemoryStore;
    }
}

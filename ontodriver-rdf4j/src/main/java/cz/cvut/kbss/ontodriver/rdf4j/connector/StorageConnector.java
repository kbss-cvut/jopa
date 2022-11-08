/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.rdf4j.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.config.Constants;
import cz.cvut.kbss.ontodriver.rdf4j.config.Rdf4jConfigParam;
import cz.cvut.kbss.ontodriver.rdf4j.config.Rdf4jOntoDriverProperties;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.query.QuerySpecification;
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
import java.util.Collections;
import java.util.List;
import java.util.Objects;

public class StorageConnector extends AbstractConnector {

    private static final Logger LOG = LoggerFactory.getLogger(StorageConnector.class);

    private final DriverConfiguration configuration;

    private final int maxReconnectAttempts;

    private Repository repository;
    private RepositoryManager manager;
    private RepositoryConnection connection;

    StorageConnector(DriverConfiguration configuration) throws Rdf4jDriverException {
        assert configuration != null;

        this.configuration = configuration;
        this.maxReconnectAttempts = resolveMaxReconnectAttempts();
        initialize();
        this.open = true;
    }

    private int resolveMaxReconnectAttempts() throws Rdf4jDriverException {
        try {
            final int attempts = configuration.isSet(Rdf4jConfigParam.RECONNECT_ATTEMPTS) ? Integer.parseInt(
                    configuration.getProperty(Rdf4jConfigParam.RECONNECT_ATTEMPTS)) :
                                 Constants.DEFAULT_RECONNECT_ATTEMPTS_COUNT;
            if (attempts < 0) {
                throw invalidReconnectAttemptsConfig();
            }
            return attempts;
        } catch (NumberFormatException e) {
            throw invalidReconnectAttemptsConfig();
        }
    }

    private static Rdf4jDriverException invalidReconnectAttemptsConfig() {
        return new Rdf4jDriverException(
                "Invalid value of configuration parameter " + Rdf4jOntoDriverProperties.RECONNECT_ATTEMPTS +
                        ". Must be a non-negative integer.");
    }

    private void initialize() throws Rdf4jDriverException {
        final RepositoryConnectorInitializer initializer = new RepositoryConnectorInitializer(configuration,
                                                                                              maxReconnectAttempts);
        initializer.initializeRepository();
        this.manager = initializer.getManager();
        this.repository = initializer.getRepository();
    }

    @Override
    public void close() throws Rdf4jDriverException {
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
            throw new Rdf4jDriverException("Exception caught when closing RDF4J repository connection.", e);
        } finally {
            this.open = false;
        }
    }

    @Override
    public TupleQueryResult executeSelectQuery(QuerySpecification query) throws Rdf4jDriverException {
        final RepositoryConnection conn = acquireConnection();
        return new ConnectionStatementExecutor(conn).executeSelectQuery(query);
        // The connection is released by the result set once it is closed
    }

    RepositoryConnection acquireConnection() throws Rdf4jDriverException {
        // Workaround for local native storage being reset when multiple drivers access it
        if (!repository.isInitialized()) {
            initialize();
        }
        LOG.trace("Acquiring repository connection.");
        return acquire(1);
    }

    private RepositoryConnection acquire(int attempts) throws Rdf4jDriverException {
        try {
            return repository.getConnection();
        } catch (RepositoryException e) {
            if (attempts < maxReconnectAttempts) {
                LOG.warn("Unable to acquire repository connection. Error is: {}. Retrying...", e.getMessage());
                return acquire(attempts + 1);
            }
            LOG.error("Threshold of failed connection acquisition attempts reached, throwing exception.");
            throw new Rdf4jDriverException(e);
        }
    }

    void releaseConnection(RepositoryConnection conn) throws Rdf4jDriverException {
        try {
            if (conn != null) {
                LOG.trace("Releasing repository connection.");
                conn.close();
            }
        } catch (RepositoryException e) {
            throw new Rdf4jDriverException(e);
        }
    }

    @Override
    public boolean executeBooleanQuery(QuerySpecification query) throws Rdf4jDriverException {
        try (final RepositoryConnection conn = acquireConnection()) {
            return new ConnectionStatementExecutor(conn).executeBooleanQuery(query);
        }
    }

    @Override
    public void executeUpdate(QuerySpecification query) throws Rdf4jDriverException {
        try (final RepositoryConnection conn = acquireConnection()) {
            new ConnectionStatementExecutor(conn).executeUpdate(query);
        }
    }

    @Override
    public List<Resource> getContexts() throws Rdf4jDriverException {
        try (final RepositoryConnection conn = acquireConnection()) {
            final RepositoryResult<Resource> res = conn.getContextIDs();
            return Iterations.asList(res);
        } catch (RepositoryException e) {
            throw new Rdf4jDriverException(e);
        }
    }

    @Override
    public ValueFactory getValueFactory() {
        return repository.getValueFactory();
    }

    @Override
    public void begin() throws Rdf4jDriverException {
        super.begin();
        this.connection = acquireConnection();
        try {
            connection.begin();
        } catch (RepositoryException e) {
            transaction.rollback();
            throw new Rdf4jDriverException(e);
        }
    }

    @Override
    public void commit() throws Rdf4jDriverException {
        assert connection != null;

        transaction.commit();
        try {
            connection.commit();
            connection.close();
            this.connection = null;
            transaction.afterCommit();
        } catch (RepositoryException e) {
            transaction.rollback();
            throw new Rdf4jDriverException(e);
        }
    }

    @Override
    public void rollback() throws Rdf4jDriverException {
        assert connection != null;
        transaction.rollback();
        try {
            connection.rollback();
            connection.close();
            this.connection = null;
        } catch (RepositoryException e) {
            throw new Rdf4jDriverException(e);
        } finally {
            transaction.afterRollback();
        }
    }

    @Override
    public void addStatements(Collection<Statement> statements) throws Rdf4jDriverException {
        verifyTransactionActive();
        assert connection != null;

        try {
            connection.add(statements);
        } catch (RepositoryException e) {
            throw new Rdf4jDriverException(e);
        }
    }

    @Override
    public void removeStatements(Collection<Statement> statements) throws Rdf4jDriverException {
        verifyTransactionActive();
        assert connection != null;

        try {
            connection.remove(statements);
        } catch (RepositoryException e) {
            throw new Rdf4jDriverException(e);
        }
    }

    @Override
    public Collection<Statement> findStatements(Resource subject, IRI property, Value value, boolean includeInferred)
            throws Rdf4jDriverException {
        return findStatements(subject, property, value, includeInferred, Collections.emptySet());
    }

    @Override
    public Collection<Statement> findStatements(Resource subject, org.eclipse.rdf4j.model.IRI property,
                                                Value value, boolean includeInferred, Collection<IRI> context)
            throws Rdf4jDriverException {
        try (final RepositoryConnection conn = acquireConnection()) {
            final RepositoryResult<Statement> m;
            m = conn.getStatements(subject, property, null, includeInferred, context.toArray(new IRI[0]));
            return Iterations.asList(m);
        } catch (RepositoryException e) {
            throw new Rdf4jDriverException(e);
        }
    }

    @Override
    public boolean containsStatement(Resource subject, IRI property, Value value, boolean includeInferred,
                                     Collection<IRI> contexts)
            throws Rdf4jDriverException {
        try (final RepositoryConnection conn = acquireConnection()) {
            return conn.hasStatement(subject, property, null, includeInferred, contexts.toArray(new IRI[0]));
        } catch (RepositoryException e) {
            throw new Rdf4jDriverException(e);
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
        throw new Rdf4jDriverException("No instance of class " + cls + " found.");
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
        Sail sail = ((SailRepository) repo).getSail();
        while (sail instanceof SailWrapper) {
            sail = ((SailWrapper) sail).getBaseSail();
        }
        return sail instanceof MemoryStore;
    }
}

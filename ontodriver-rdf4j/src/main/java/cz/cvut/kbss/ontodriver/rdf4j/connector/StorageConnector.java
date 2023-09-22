/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.rdf4j.connector;

import cz.cvut.kbss.ontodriver.Wrapper;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.connector.init.RepositoryConnectorInitializer;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.query.QuerySpecification;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.Value;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;
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
import java.util.Set;
import java.util.stream.Collectors;

public class StorageConnector extends AbstractConnector {

    private static final Logger LOG = LoggerFactory.getLogger(StorageConnector.class);

    private final int maxReconnectAttempts;

    private Repository repository;
    private final RepositoryManager manager;
    private RepositoryConnection connection;

    public StorageConnector(RepositoryConnectorInitializer repoInitializer) {
        this.repository = repoInitializer.getRepository();
        this.manager = repoInitializer.getManager();
        this.maxReconnectAttempts = repoInitializer.getMaxReconnectAttempts();
        this.open = true;
    }

    @Override
    public void close() throws Rdf4jDriverException {
        if (!open) {
            return;
        }
        LOG.debug("Closing repository connector.");
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
            repository.init();
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
            return conn.getContextIDs().stream().collect(Collectors.toList());
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
    public void removePropertyValues(Collection<SubjectPredicateContext> spc) throws Rdf4jDriverException {
        verifyTransactionActive();
        assert connection != null;

        try {
            spc.forEach(spcItem -> connection.remove(spcItem.getSubject(), spcItem.getPredicate(), null, spcItem.getContexts()
                                                                                                                .toArray(Resource[]::new)));
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
                                                Value value, boolean includeInferred, Set<IRI> context)
            throws Rdf4jDriverException {
        try (final RepositoryConnection conn = acquireConnection()) {
            return conn.getStatements(subject, property, null, includeInferred, context.toArray(new IRI[0])).stream()
                       .collect(Collectors.toList());
        } catch (RepositoryException e) {
            throw new Rdf4jDriverException(e);
        }
    }

    @Override
    public boolean containsStatement(Resource subject, IRI property, Value value, boolean includeInferred,
                                     Set<IRI> contexts) throws Rdf4jDriverException {
        assert contexts != null;
        try (final RepositoryConnection conn = acquireConnection()) {
            return conn.hasStatement(subject, property, value, includeInferred, contexts.toArray(new IRI[0]));
        } catch (RepositoryException e) {
            throw new Rdf4jDriverException(e);
        }
    }

    @Override
    public boolean isInferred(Statement statement, Set<IRI> contexts) throws Rdf4jDriverException {
        assert contexts != null;
        try (final RepositoryConnection conn = acquireConnection()) {
            final IRI[] ctxArr = contexts.toArray(new IRI[0]);
            return conn.hasStatement(statement, true, ctxArr) && !conn.hasStatement(statement, false, ctxArr);
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
        if (repository instanceof Wrapper) {
            return ((Wrapper) repository).unwrap(cls);
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

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
package cz.cvut.kbss.ontodriver.rdf4j.connector;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.query.QuerySpecification;
import cz.cvut.kbss.ontodriver.rdf4j.util.ThrowingFunction;
import org.eclipse.rdf4j.common.transaction.IsolationLevel;
import org.eclipse.rdf4j.common.transaction.IsolationLevels;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.Value;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class StorageConnection implements RepoConnection {

    private static final Logger LOG = LoggerFactory.getLogger(StorageConnection.class);

    private boolean open;
    private boolean readOnly;

    final Rdf4jConnectionProvider connectionProvider;
    private final IsolationLevel isolationLevel;
    private RepositoryConnection connection;

    public StorageConnection(Rdf4jConnectionProvider connectionProvider, IsolationLevel isolationLevel) {
        this.connectionProvider = connectionProvider;
        this.isolationLevel = isolationLevel;
        this.open = true;
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    @Override
    public void close() throws Rdf4jDriverException {
        if (!open) {
            return;
        }
        LOG.debug("Closing repository connector.");
        try {
            if (connection != null) {
                if (connection.isActive()) {
                    connection.rollback();
                }
                connection.close();
            }
        } catch (RepositoryException e) {
            throw new Rdf4jDriverException("Exception caught when closing RDF4J repository connection.", e);
        } finally {
            this.open = false;
        }
    }

    @Override
    public void setReadOnly(boolean readOnly) {
        if (connection != null && connection.isActive()) {
            throw new IllegalStateException("Cannot set read-only mode during active repository transaction.");
        }
        this.readOnly = readOnly;
    }

    @Override
    public boolean isReadOnly() {
        return readOnly;
    }

    @Override
    public TupleQueryResult executeSelectQuery(QuerySpecification query) throws Rdf4jDriverException {
        // Always create a separate connection, it is released by the result set once it is closed
        return new ConnectionStatementExecutor(connectionProvider.acquireConnection()).executeSelectQuery(query);
    }

    @Override
    public boolean executeBooleanQuery(QuerySpecification query) throws Rdf4jDriverException {
        return withConnection(conn -> new ConnectionStatementExecutor(conn).executeBooleanQuery(query));
    }

    protected <R> R withConnection(ThrowingFunction<RepositoryConnection, R> call) throws Rdf4jDriverException {
        if (connection != null) {
            return call.apply(connection);
        } else {
            try (final RepositoryConnection conn = connectionProvider.acquireConnection()) {
                return call.apply(conn);
            }
        }
    }

    @Override
    public void executeUpdate(QuerySpecification query) throws Rdf4jDriverException {
        if (connection != null) {
            new ConnectionStatementExecutor(connection).executeUpdate(query);
        } else {
            try (final RepositoryConnection conn = connectionProvider.acquireConnection()) {
                new ConnectionStatementExecutor(conn).executeUpdate(query);
            }
        }
    }

    @Override
    public List<Resource> getContexts() throws Rdf4jDriverException {
        return withConnection(conn -> {
            try {
                return conn.getContextIDs().stream().collect(Collectors.toList());
            } catch (RepositoryException e) {
                throw new Rdf4jDriverException(e);
            }
        });
    }

    @Override
    public ValueFactory getValueFactory() {
        return connectionProvider.getValueFactory();
    }

    @Override
    public void begin() throws Rdf4jDriverException {
        this.connection = connectionProvider.acquireConnection();
        if (shouldBeginRepositoryLevelTransaction()) {
            try {
                LOG.trace("Begin storage transaction.");
                connection.begin(isolationLevel);
            } catch (RepositoryException e) {
                throw new Rdf4jDriverException(e);
            }
        } else {
            LOG.trace("Read-only connection, not starting repository-level transaction");
        }
    }

    boolean shouldBeginRepositoryLevelTransaction() {
        return !readOnly || isolationLevel == IsolationLevels.SERIALIZABLE;
    }

    @Override
    public void commit() throws Rdf4jDriverException {
        assert connection != null;

        try {
            if (!readOnly) {
                LOG.trace("Commit storage transaction.");
                connection.commit();
            }
            connection.close();
            this.connection = null;
        } catch (RepositoryException e) {
            throw new Rdf4jDriverException(e);
        }
    }

    @Override
    public void rollback() throws Rdf4jDriverException {
        assert connection != null;
        try {
            LOG.trace("Rollback storage transaction.");
            connection.rollback();
            connection.close();
            this.connection = null;
        } catch (RepositoryException e) {
            throw new Rdf4jDriverException(e);
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

    protected void verifyTransactionActive() {
        if (connection == null || !connection.isActive()) {
            throw new IllegalStateException();
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
            spc.forEach(spcItem -> connection.remove(spcItem.subject(), spcItem.predicate(), null, spcItem.contexts()
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
        return withConnection(conn -> {
            try {
                return conn.getStatements(subject, property, null, includeInferred, context.toArray(new IRI[0]))
                           .stream()
                           .collect(Collectors.toList());
            } catch (RepositoryException e) {
                throw new Rdf4jDriverException(e);
            }
        });
    }

    @Override
    public boolean containsStatement(Resource subject, IRI property, Value value, boolean includeInferred,
                                     Set<IRI> contexts) throws Rdf4jDriverException {
        assert contexts != null;
        return withConnection(conn -> {
            try {
                return conn.hasStatement(subject, property, value, includeInferred, contexts.toArray(new IRI[0]));
            } catch (RepositoryException e) {
                throw new Rdf4jDriverException(e);
            }
        });
    }

    @Override
    public boolean isInferred(Statement statement, Set<IRI> contexts) throws Rdf4jDriverException {
        assert contexts != null;
        return withConnection(conn -> {
            try {
                final IRI[] ctxArr = contexts.toArray(new IRI[0]);
                return conn.hasStatement(statement, true, ctxArr) && !conn.hasStatement(statement, false, ctxArr);
            } catch (RepositoryException e) {
                throw new Rdf4jDriverException(e);
            }
        });
    }

    @Override
    public <T> T unwrap(Class<T> cls) throws OntoDriverException {
        if (cls.isAssignableFrom(getClass())) {
            return cls.cast(this);
        }
        if (cls.isAssignableFrom(RepositoryConnection.class)) {
            return cls.cast(connection);
        }
        return connectionProvider.unwrap(cls);
    }

    @Override
    public String getProductName() {
        return "Virtuoso";
    }
}

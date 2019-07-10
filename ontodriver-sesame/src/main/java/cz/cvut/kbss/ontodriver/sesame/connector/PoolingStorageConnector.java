/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.sesame.connector;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import org.eclipse.rdf4j.common.iteration.Iterations;
import org.eclipse.rdf4j.model.*;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

class PoolingStorageConnector extends AbstractConnector {

    private static final ReentrantReadWriteLock LOCK = new ReentrantReadWriteLock();
    private static final Lock READ = LOCK.readLock();
    private static final Lock WRITE = LOCK.writeLock();

    private final StorageConnector centralConnector;

    private RepositoryConnection connection;
    private LocalModel localModel;

    PoolingStorageConnector(StorageConnector centralConnector) {
        this.centralConnector = centralConnector;
        this.open = true;
    }

    @Override
    public TupleQueryResult executeSelectQuery(String query) throws SesameDriverException {
        if (transaction.isActive()) {
            return new ConnectionStatementExecutor(wrapConnection()).executeSelectQuery(query);
        }
        READ.lock();
        try {
            return centralConnector.executeSelectQuery(query);
        } finally {
            READ.unlock();
        }
    }

    private RepositoryConnection wrapConnection() {
        return new TransactionalRepositoryConnection(connection);
    }

    @Override
    public boolean executeBooleanQuery(String query) throws SesameDriverException {
        if (transaction.isActive()) {
            return new ConnectionStatementExecutor(wrapConnection()).executeBooleanQuery(query);
        }
        READ.lock();
        try {
            return centralConnector.executeBooleanQuery(query);
        } finally {
            READ.unlock();
        }
    }

    @Override
    public void executeUpdate(String query) throws SesameDriverException {
        WRITE.lock();
        try {
            centralConnector.executeUpdate(query);
        } finally {
            WRITE.unlock();
        }
    }

    @Override
    public List<Resource> getContexts() throws SesameDriverException {
        READ.lock();
        try {
            return centralConnector.getContexts();
        } finally {
            READ.unlock();
        }
    }

    @Override
    public ValueFactory getValueFactory() {
        // We don't need to lock the central connector, as getting the value
        // factory does not require communication with the repository
        return centralConnector.getValueFactory();
    }

    @Override
    public void begin() throws SesameDriverException {
        super.begin();
        this.localModel = new LocalModel();
        this.connection = centralConnector.acquireConnection();
    }

    @Override
    public void commit() throws SesameDriverException {
        transaction.commit();
        WRITE.lock();
        try {
            centralConnector.begin();
            centralConnector.removeStatements(localModel.getRemovedStatements());
            centralConnector.addStatements(localModel.getAddedStatements());
            centralConnector.commit();
            transaction.afterCommit();
        } catch (SesameDriverException e) {
            transaction.rollback();
            centralConnector.rollback();
            transaction.afterRollback();
            throw e;
        } finally {
            WRITE.unlock();
            centralConnector.releaseConnection(connection);
            this.localModel = null;
        }
    }

    @Override
    public void rollback() throws SesameDriverException {
        transaction.rollback();
        this.localModel = null;
        centralConnector.releaseConnection(connection);
        transaction.afterRollback();
    }

    @Override
    public void close() throws OntoDriverException {
        if (open && transaction.isActive()) {
            this.localModel = null;
            centralConnector.releaseConnection(connection);
        }
        super.close();
    }

    @Override
    public void addStatements(Collection<Statement> statements) {
        verifyTransactionActive();
        assert statements != null;
        localModel.addStatements(statements);
    }

    @Override
    public void removeStatements(Collection<Statement> statements) {
        verifyTransactionActive();
        assert statements != null;
        localModel.removeStatements(statements);
    }

    @Override
    public Collection<Statement> findStatements(Resource subject, IRI property, Value value, boolean includeInferred)
            throws SesameDriverException {
        return findStatements(subject, property, value, includeInferred, null);
    }

    @Override
    public Collection<Statement> findStatements(Resource subject, IRI property, Value value,
                                                boolean includeInferred, IRI context) throws SesameDriverException {
        verifyTransactionActive();
        try {
            final Collection<Statement> statements;
            if (context != null) {
                statements = Iterations
                        .asList(connection.getStatements(subject, property, value, includeInferred, context));
            } else {
                statements = Iterations.asList(connection.getStatements(subject, property, value, includeInferred));
            }
            localModel.enhanceStatements(statements, subject, property, value, context);
            return statements;
        } catch (RepositoryException e) {
            rollback();
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
        verifyTransactionActive();
        try {
            final LocalModel.Contains containsLocally = localModel.contains(subject, property, value, context);
            switch (containsLocally) {
                case TRUE:
                    return true;
                case FALSE:
                    return false;
                default:
                    if (context != null) {
                        return connection.hasStatement(subject, property, value, includeInferred, context);
                    } else {
                        return connection.hasStatement(subject, property, value, includeInferred);
                    }
            }
        } catch (RepositoryException e) {
            rollback();
            throw new SesameDriverException(e);
        }
    }

    @Override
    public <T> T unwrap(Class<T> cls) throws OntoDriverException {
        if (cls.isAssignableFrom(this.getClass())) {
            return cls.cast(this);
        }
        return centralConnector.unwrap(cls);
    }
}

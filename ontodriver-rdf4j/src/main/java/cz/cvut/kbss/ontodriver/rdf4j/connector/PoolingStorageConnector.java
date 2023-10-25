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
package cz.cvut.kbss.ontodriver.rdf4j.connector;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.query.QuerySpecification;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.Value;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.stream.Stream;

public class PoolingStorageConnector extends AbstractConnector {

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
    public TupleQueryResult executeSelectQuery(QuerySpecification query) throws Rdf4jDriverException {
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
    public boolean executeBooleanQuery(QuerySpecification query) throws Rdf4jDriverException {
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
    public void executeUpdate(QuerySpecification query) throws Rdf4jDriverException {
        WRITE.lock();
        try {
            centralConnector.executeUpdate(query);
        } finally {
            WRITE.unlock();
        }
    }

    @Override
    public List<Resource> getContexts() throws Rdf4jDriverException {
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
    public void begin() throws Rdf4jDriverException {
        super.begin();
        this.localModel = new LocalModel();
        this.connection = centralConnector.acquireConnection();
    }

    @Override
    public void commit() throws Rdf4jDriverException {
        transaction.commit();
        WRITE.lock();
        try {
            centralConnector.begin();
            centralConnector.removeStatements(localModel.getRemovedStatements());
            centralConnector.removePropertyValues(localModel.getRemovedSubjectPredicateStatements());
            centralConnector.addStatements(localModel.getAddedStatements());
            centralConnector.commit();
            transaction.afterCommit();
        } catch (Rdf4jDriverException e) {
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
    public void rollback() throws Rdf4jDriverException {
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
    public void removePropertyValues(Collection<SubjectPredicateContext> spc) {
        localModel.removePropertyValues(spc);
    }

    @Override
    public Collection<Statement> findStatements(Resource subject, IRI property, Value value, boolean includeInferred)
            throws Rdf4jDriverException {
        return findStatements(subject, property, value, includeInferred, Collections.emptySet());
    }

    @Override
    public Collection<Statement> findStatements(Resource subject, IRI property, Value value,
                                                boolean includeInferred, Set<IRI> contexts)
            throws Rdf4jDriverException {
        verifyTransactionActive();
        try {
            final Stream<Statement> statements = connection
                    .getStatements(subject, property, value, includeInferred,
                            contexts.toArray(new IRI[0])).stream();
            return localModel.enhanceStatements(statements, subject, property, value, contexts);
        } catch (RepositoryException e) {
            rollback();
            throw new Rdf4jDriverException(e);
        }
    }

    @Override
    public boolean containsStatement(Resource subject, IRI property, Value value, boolean includeInferred,
                                     Set<IRI> contexts)
            throws Rdf4jDriverException {
        verifyTransactionActive();
        try {
            final LocalModel.Contains containsLocally = localModel.contains(subject, property, value, contexts);
            switch (containsLocally) {
                case TRUE:
                    return true;
                case FALSE:
                    return false;
                default:
                    return connection
                            .hasStatement(subject, property, value, includeInferred, contexts.toArray(new IRI[0]));
            }
        } catch (RepositoryException e) {
            rollback();
            throw new Rdf4jDriverException(e);
        }
    }

    @Override
    public boolean isInferred(Statement statement, Set<IRI> contexts) throws Rdf4jDriverException {
        verifyTransactionActive();
        return centralConnector.isInferred(statement, contexts);
    }

    @Override
    public <T> T unwrap(Class<T> cls) throws OntoDriverException {
        if (cls.isAssignableFrom(this.getClass())) {
            return cls.cast(this);
        }
        return centralConnector.unwrap(cls);
    }
}

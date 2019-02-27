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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.exceptions.NoUniqueResultException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.query.QueryHolder;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.jopa.utils.Procedure;
import cz.cvut.kbss.jopa.utils.ThrowingConsumer;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

/**
 * Common state and behavior of both {@link cz.cvut.kbss.jopa.model.query.Query} and {@link
 * cz.cvut.kbss.jopa.model.query.TypedQuery} implementations.
 */
abstract class AbstractQuery implements Query {

    private static final Logger LOG = LoggerFactory.getLogger(AbstractQuery.class);

    final QueryHolder query;
    private final ConnectionWrapper connection;

    private boolean useBackupOntology = false;

    private Procedure rollbackOnlyMarker;
    private Procedure ensureOpenProcedure;

    AbstractQuery(QueryHolder query, ConnectionWrapper connection) {
        this.query = Objects.requireNonNull(query, ErrorUtils.getNPXMessageSupplier("query"));
        this.connection = Objects.requireNonNull(connection, ErrorUtils.getNPXMessageSupplier("connection"));
    }

    /**
     * Sets ontology used for processing of this query.
     *
     * @param useBackupOntology If true, the backup (central) ontology is used, otherwise the transactional ontology is
     *                          used (default)
     */
    public void useBackupOntology(boolean useBackupOntology) {
        this.useBackupOntology = useBackupOntology;
    }

    private void logQuery() {
        if (LOG.isTraceEnabled()) {
            LOG.trace("Executing query: {}", query.assembleQuery());
        }
    }

    private void setTargetOntology(Statement stmt) {
        if (useBackupOntology) {
            stmt.useOntology(Statement.StatementOntology.CENTRAL);
        } else {
            stmt.useOntology(Statement.StatementOntology.TRANSACTIONAL);
        }
    }

    OWLPersistenceException queryEvaluationException(OntoDriverException e) {
        final String executedQuery = query.assembleQuery();
        return new OWLPersistenceException("Exception caught when evaluating query " + executedQuery, e);
    }

    void markTransactionForRollback() {
        if (rollbackOnlyMarker != null) {
            rollbackOnlyMarker.execute();
        }
    }

    void ensureOpen() {
        assert ensureOpenProcedure != null;
        ensureOpenProcedure.execute();
    }

    /**
     * Registers reference to a method which marks current transaction (if active) for rollback on exceptions.
     *
     * @param rollbackOnlyMarker The marker to invoke on exceptions
     */
    void setRollbackOnlyMarker(Procedure rollbackOnlyMarker) {
        this.rollbackOnlyMarker = rollbackOnlyMarker;
    }

    /**
     * Registers a reference to a method which ensures that the query is called on an open persistence context.
     * <p>
     * This method is likely to come from an {@link EntityManager} instance which was used to create this query.
     *
     * @param ensureOpenProcedure The procedure to call when ensuring that persistence context is open
     */
    void setEnsureOpenProcedure(Procedure ensureOpenProcedure) {
        this.ensureOpenProcedure = ensureOpenProcedure;
    }

    private static IllegalStateException unboundParam(Object param) {
        return new IllegalStateException("Parameter " + param + " is not bound.");
    }

    @Override
    public void executeUpdate() {
        ensureOpen();
        final Statement stmt = connection.createStatement();
        try {
            setTargetOntology(stmt);
            logQuery();
            stmt.executeUpdate(query.assembleQuery());
        } catch (OntoDriverException e) {
            markTransactionForRollback();
            throw queryEvaluationException(e);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        } finally {
            try {
                stmt.close();
            } catch (Exception e) {
                LOG.error("Unable to close statement after update execution.", e);
            }
        }
    }

    @Override
    public Parameter<?> getParameter(int position) {
        return query.getParameter(position);
    }

    @Override
    public Parameter<?> getParameter(String name) {
        return query.getParameter(name);
    }

    @Override
    public Set<Parameter<?>> getParameters() {
        return query.getParameters();
    }

    @Override
    public boolean isBound(Parameter<?> parameter) {
        return query.getParameterValue(parameter) != null;
    }

    @Override
    public Object getParameterValue(String name) {
        final Parameter<?> param = query.getParameter(name);
        return getParameterValue(param);
    }

    @Override
    public Object getParameterValue(int position) {
        final Parameter<?> param = query.getParameter(position);
        return getParameterValue(param);
    }

    @Override
    public <T> T getParameterValue(Parameter<T> parameter) {
        if (!isBound(parameter)) {
            throw unboundParam(parameter);
        }
        return (T) query.getParameterValue(parameter);
    }

     @Override
    public Query setParameter(int position, Object value) {
        ensureOpen();
        try {
            query.setParameter(query.getParameter(position), value);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
        return this;
    }

    @Override
    public Query setParameter(int position, String value, String language) {
        ensureOpen();
        try {
            query.setParameter(query.getParameter(position), value, language);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
        return this;
    }

    @Override
    public Query setParameter(String name, Object value) {
        ensureOpen();
        try {
            query.setParameter(query.getParameter(name), value);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
        return this;
    }

    @Override
    public Query setParameter(String name, String value, String language) {
        ensureOpen();
        try {
            query.setParameter(query.getParameter(name), value, language);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
        return this;
    }

    @Override
    public Query setUntypedParameter(int position, Object value) {
        ensureOpen();
        try {
            query.setUntypedParameter(query.getParameter(position), value);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
        return this;
    }

    @Override
    public Query setUntypedParameter(String name, Object value) {
        ensureOpen();
        try {
            query.setUntypedParameter(query.getParameter(name), value);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
        return this;
    }

    @Override
    public int getMaxResults() {
        return query.getMaxResults();
    }

    @Override
    public int getFirstResult() {
        return query.getFirstResult();
    }

    void checkNumericParameter(int param, String name) {
        if (param < 0) {
            markTransactionForRollback();
            throw new IllegalArgumentException("Cannot set " + name + " to less than 0.");
        }
    }

    /**
     * Executes the query and lets the specified consumer deal with each row in the result set.
     *
     * @param consumer Called for every row in the result set
     * @throws OntoDriverException When something goes wrong during query evaluation or result set processing
     */
    void executeQuery(ThrowingConsumer<ResultRow, OntoDriverException> consumer) throws OntoDriverException {
        try (final Statement stmt = connection.createStatement()) {
            setTargetOntology(stmt);
            logQuery();
            final ResultSet rs = stmt.executeQuery(query.assembleQuery());
            for (ResultRow row : rs) {
                consumer.accept(row);
            }
        }
    }

    <R> Stream<R> executeQueryForStream(Function<ResultRow, Optional<R>> function) throws OntoDriverException {
        final Statement stmt = connection.createStatement();
        setTargetOntology(stmt);
        logQuery();
        final ResultSet rs = stmt.executeQuery(query.assembleQuery());
        return StreamSupport.stream(new QueryResultSpliterator<R>(rs.spliterator(), function, () -> {
            try {
                stmt.close();
            } catch (OntoDriverException e) {
                markTransactionForRollback();
                throw new OWLPersistenceException(e);
            }
        }), false);
    }

    boolean exceptionCausesRollback(RuntimeException e) {
        return !(e instanceof NoUniqueResultException) && !(e instanceof NoResultException);
    }

    @Override
    public <T> Query setParameter(Parameter<T> parameter, T value) {
        ensureOpen();
        try {
            query.setParameter(parameter, value);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
        return this;
    }

    @Override
    public Query setParameter(Parameter<String> parameter, String value, String language) {
        ensureOpen();
        try {
            query.setParameter(parameter, value, language);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
        return this;
    }

    @Override
    public <T> Query setUntypedParameter(Parameter<T> parameter, T value) {
        ensureOpen();
        try {
            query.setUntypedParameter(parameter, value);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
        return this;
    }
}

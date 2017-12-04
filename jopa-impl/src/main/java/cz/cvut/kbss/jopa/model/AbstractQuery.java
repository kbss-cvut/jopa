/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;
import java.util.Set;

/**
 * Common state and behavior of both {@link cz.cvut.kbss.jopa.model.query.Query} and {@link
 * cz.cvut.kbss.jopa.model.query.TypedQuery} implementations.
 */
abstract class AbstractQuery implements Query {

    private static final Logger LOG = LoggerFactory.getLogger(AbstractQuery.class);

    final QueryHolder query;
    private final ConnectionWrapper connection;
    int firstResult = 0;
    int maxResults = Integer.MAX_VALUE;

    private boolean useBackupOntology = false;

    private Procedure rollbackOnlyMarker;

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

    void executeUpdateImpl() {
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

    /**
     * Registers reference to a method which marks current transaction (if active) for rollback on exceptions.
     *
     * @param rollbackOnlyMarker The marker to invoke on exceptions
     */
    void setRollbackOnlyMarker(Procedure rollbackOnlyMarker) {
        this.rollbackOnlyMarker = rollbackOnlyMarker;
    }

    private static IllegalStateException unboundParam(Object param) {
        return new IllegalStateException("Parameter " + param + " is not bound.");
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
    public int getMaxResults() {
        return maxResults;
    }

    @Override
    public int getFirstResult() {
        return firstResult;
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
    void executeQuery(ThrowingConsumer<ResultSet, OntoDriverException> consumer) throws OntoDriverException {
        assert maxResults > 0;

        final Statement stmt = connection.createStatement();
        try {
            setTargetOntology(stmt);
            logQuery();
            final ResultSet rs = stmt.executeQuery(query.assembleQuery());
            int cnt = 0;
            int index = 0;
            // TODO register this as observer on the result set so that additional results can be loaded asynchronously
            while (rs.hasNext() && cnt < maxResults) {
                rs.next();
                if (index >= firstResult) {
                    consumer.accept(rs);
                    cnt++;
                }
                index++;
            }
        } finally {
            try {
                stmt.close();
            } catch (Exception e) {
                LOG.error("Unable to close statement after query evaluation.", e);
            }
        }
    }
}

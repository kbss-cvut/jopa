/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
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
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.query.QueryHolder;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.util.*;

public class TypedQueryImpl<X> implements TypedQuery<X> {

    private static final Logger LOG = LoggerFactory.getLogger(TypedQueryImpl.class);

    private final QueryHolder query;
    private final Set<URI> contexts;
    private final Class<X> resultType;
    private final ConnectionWrapper connection;
    private final MetamodelProvider metamodelProvider;

    private UnitOfWork uow;

    private boolean useBackupOntology;
    private int maxResults;

    private Runnable rollbackOnlyMarker;

    public TypedQueryImpl(final QueryHolder query, final Class<X> resultType,
                          final ConnectionWrapper connection, MetamodelProvider metamodelProvider) {
        this.query = Objects.requireNonNull(query, ErrorUtils.getNPXMessageSupplier("query"));
        this.resultType = Objects.requireNonNull(resultType, ErrorUtils.getNPXMessageSupplier("resultType"));
        this.connection = Objects.requireNonNull(connection, ErrorUtils.getNPXMessageSupplier("connection"));
        this.metamodelProvider = Objects
                .requireNonNull(metamodelProvider, ErrorUtils.getNPXMessageSupplier("metamodelProvider"));
        this.contexts = new HashSet<>();
        this.maxResults = Integer.MAX_VALUE;
    }

    public void setUnitOfWork(UnitOfWork uow) {
        this.uow = uow;
    }

    @Override
    public void executeUpdate() {
        final Statement stmt = connection.createStatement();
        try {
        URI[] uris = new URI[contexts.size()];
        uris = contexts.toArray(uris);
        setTargetOntology(stmt);
            stmt.executeUpdate(query.assembleQuery(), uris);
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

    private void markTransactionForRollback() {
        if (rollbackOnlyMarker != null) {
            rollbackOnlyMarker.run();
        }
    }

    @Override
    public List<X> getResultList() {
        if (maxResults == 0) {
            return Collections.emptyList();
        }
        List<X> list;
        try {
            list = getResultListImpl(maxResults);
        } catch (OntoDriverException e) {
            markTransactionForRollback();
            throw queryEvaluationException(e);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }

        return list;
    }

    private OWLPersistenceException queryEvaluationException(OntoDriverException e) {
        final String executedQuery = query.assembleQuery();
        return new OWLPersistenceException("Exception caught when evaluating query " + executedQuery, e);
    }

    @Override
    public X getSingleResult() {
        try {
            // call it with maxResults = 2 just to see whether there are
            // multiple results
            final List<X> res = getResultListImpl(2);
            if (res.isEmpty()) {
                throw new NoResultException("No result found for query " + query);
            }
            if (res.size() > 1) {
                throw new NoUniqueResultException("Multiple results found for query " + query);
            }
            return res.get(0);
        } catch (OntoDriverException e) {
            markTransactionForRollback();
            throw queryEvaluationException(e);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
    }

    @Override
    public TypedQuery<X> setMaxResults(int maxResults) {
        if (maxResults < 0) {
            markTransactionForRollback();
            throw new IllegalArgumentException("Cannot set maximum number of results to less than 0.");
        }
        this.maxResults = maxResults;
        return this;
    }

    @Override
    public int getMaxResults() {
        return maxResults;
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
    public Object getParameterValue(int position) {
        final Parameter<?> param = query.getParameter(position);
        return getParameterValue(param);
    }

    @Override
    public Object getParameterValue(String name) {
        final Parameter<?> param = query.getParameter(name);
        return getParameterValue(param);
    }

    private static IllegalStateException unboundParam(Object param) {
        return new IllegalStateException("Parameter " + param + " is not bound.");
    }

    @Override
    public <T> T getParameterValue(Parameter<T> parameter) {
        if (!isBound(parameter)) {
            throw unboundParam(parameter);
        }
        return (T) query.getParameterValue(parameter);
    }

    @Override
    public TypedQuery<X> setParameter(int position, Object value) {
        try {
            query.setParameter(query.getParameter(position), value);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
        return this;
    }

    @Override
    public TypedQuery<X> setParameter(int position, String value, String language) {
        try {
            query.setParameter(query.getParameter(position), value, language);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
        return this;
    }

    @Override
    public TypedQuery<X> setParameter(String name, Object value) {
        try {
            query.setParameter(query.getParameter(name), value);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
        return this;
    }

    @Override
    public TypedQuery<X> setParameter(String name, String value, String language) {
        try {
            query.setParameter(query.getParameter(name), value, language);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
        return this;
    }

    @Override
    public <T> TypedQuery<X> setParameter(Parameter<T> parameter, T value) {
        try {
            query.setParameter(parameter, value);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
        return this;
    }

    @Override
    public TypedQuery<X> setParameter(Parameter<String> parameter, String value, String language) {
        try {
            query.setParameter(parameter, value, language);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
        return this;
    }

    private List<X> getResultListImpl(int maxResults) throws OntoDriverException {
        assert maxResults > 0;
        final Statement stmt = connection.createStatement();
        try {
            setTargetOntology(stmt);
            URI[] arr = new URI[contexts.size()];
            arr = contexts.toArray(arr);
            final ResultSet rs = stmt.executeQuery(query.assembleQuery(), arr);
            final List<X> res = new ArrayList<>();
            // TODO register this as observer on the result set so that additional results can be loaded asynchronously
            int cnt = 0;
            final URI ctx = arr.length > 0 ? arr[0] : null;
            final boolean isTypeManaged = metamodelProvider.isTypeManaged(resultType);
            while (rs.hasNext() && cnt < maxResults) {
                rs.next();
                if (isTypeManaged) {
                    loadEntityInstance(rs, ctx).ifPresent(res::add);
                } else {
                    res.add(loadResultValue(rs));
                }
                cnt++;
            }
            return res;
        } finally {
            try {
                stmt.close();
            } catch (Exception e) {
                LOG.error("Unable to close statement after query evaluation.", e);
            }
        }
    }

    private void setTargetOntology(Statement stmt) {
        if (useBackupOntology) {
            stmt.useOntology(Statement.StatementOntology.CENTRAL);
        } else {
            stmt.useOntology(Statement.StatementOntology.TRANSACTIONAL);
        }
    }

    private Optional<X> loadEntityInstance(ResultSet resultSet, URI context) throws OntoDriverException {
        if (uow == null) {
            throw new IllegalStateException("Cannot load entity instance without Unit of Work.");
        }
        final URI uri = URI.create(resultSet.getString(0));
        // TODO Setting the context like this won't work for queries over multiple contexts
        final EntityDescriptor descriptor = new EntityDescriptor(context);

        return Optional.ofNullable(uow.readObject(resultType, uri, descriptor));
    }

    private X loadResultValue(ResultSet resultSet) throws OntoDriverException {
        try {
            return resultSet.getObject(0, resultType);
        } catch (OntoDriverException e) {
            throw new OWLPersistenceException("Unable to map the query result to class " + resultType, e);
        }
    }

    @Override
    public TypedQuery<X> addContext(URI context) {
        Objects.requireNonNull(context);
        contexts.add(context);
        return this;
    }

    @Override
    public TypedQuery<X> addContexts(Collection<URI> contexts) {
        Objects.requireNonNull(contexts);
        this.contexts.addAll(contexts);
        return this;
    }

    @Override
    public TypedQuery<X> clearContexts() {
        contexts.clear();
        return this;
    }

    /**
     * Sets ontology used for processing of this query.
     *
     * @param useBackupOntology If true, the backup (central) ontology is used, otherwise the transactional ontology is
     *                          used (default)
     */
    public void setUseBackupOntology(boolean useBackupOntology) {
        this.useBackupOntology = useBackupOntology;
    }

    /**
     * Registers reference to a method which marks current transaction (if active) for rollback on exceptions.
     *
     * @param rollbackOnlyMarker The marker to invoke on exceptions
     */
    void setRollbackOnlyMarker(Runnable rollbackOnlyMarker) {
        this.rollbackOnlyMarker = rollbackOnlyMarker;
    }
}

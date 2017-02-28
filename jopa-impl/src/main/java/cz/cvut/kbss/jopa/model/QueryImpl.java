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
import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.query.QueryHolder;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

import java.net.URI;
import java.util.*;

public class QueryImpl implements Query {

    private final QueryHolder query;
    private final Set<URI> contexts;
    private final ConnectionWrapper connection;

    private int maxResults;
    private boolean useBackupOntology;

    public QueryImpl(final QueryHolder query, final ConnectionWrapper connection) {
        this.query = Objects.requireNonNull(query, ErrorUtils.constructNPXMessage("query"));
        this.connection = Objects.requireNonNull(connection,
                ErrorUtils.constructNPXMessage("connection"));
        this.contexts = new HashSet<>();
        this.useBackupOntology = false;
        this.maxResults = Integer.MAX_VALUE;
    }

    @Override
    public void executeUpdate() {
        final Statement stmt = connection.createStatement();
        setTargetOntology(stmt);
        URI[] uris = new URI[contexts.size()];
        uris = contexts.toArray(uris);
        try {
            stmt.executeUpdate(query.assembleQuery(), uris);
        } catch (OntoDriverException e) {
            throw queryEvaluationException(e);
        }
    }

    @Override
    public List getResultList() {
        try {
            if (maxResults == 0) {
                return Collections.emptyList();
            }
            return getResultListImpl(maxResults);
        } catch (OntoDriverException e) {
            throw queryEvaluationException(e);
        }
    }

    private OWLPersistenceException queryEvaluationException(OntoDriverException e) {
        final String executedQuery = query.assembleQuery();
        return new OWLPersistenceException("Exception caught when evaluating query " + executedQuery, e);
    }

    @Override
    public Object getSingleResult() {
        try {
            // Call it with maxResults = 2 just to see whether there are more
            final List<?> list = getResultListImpl(2);
            if (list.isEmpty()) {
                throw new NoResultException("No result found for query " + query);
            }
            if (list.size() > 1) {
                throw new NoUniqueResultException("Multiple results found for query " + query);
            }
            return list.get(0);
        } catch (OntoDriverException e) {
            throw queryEvaluationException(e);
        }
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
    public Query setParameter(int position, Object value) {
        query.setParameter(query.getParameter(position), value);
        return this;
    }

    @Override
    public Query setParameter(int position, String value, String language) {
        query.setParameter(query.getParameter(position), value, language);
        return this;
    }

    @Override
    public Query setParameter(String name, Object value) {
        query.setParameter(query.getParameter(name), value);
        return this;
    }

    @Override
    public Query setParameter(String name, String value, String language) {
        query.setParameter(query.getParameter(name), value, language);
        return this;
    }

    @Override
    public <T> Query setParameter(Parameter<T> parameter, T value) {
        query.setParameter(parameter, value);
        return this;
    }

    @Override
    public Query setParameter(Parameter<String> parameter, String value, String language) {
        query.setParameter(parameter, value, language);
        return this;
    }

    @Override
    public Query setMaxResults(int maxResults) {
        if (maxResults < 0) {
            throw new IllegalArgumentException(
                    "Cannot set maximum number of results to less than 0.");
        }
        this.maxResults = maxResults;
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

    private List<?> getResultListImpl(int maxResults) throws OntoDriverException {
        assert maxResults > 0;

        final Statement stmt = connection.createStatement();
        setTargetOntology(stmt);
        URI[] uris = new URI[contexts.size()];
        uris = contexts.toArray(uris);
        try (ResultSet rs = stmt.executeQuery(query.assembleQuery(), uris)) {
            final int columnCount = rs.getColumnCount();
            int cnt = 0;
            final List<Object> res = new ArrayList<>();
            // TODO register this as observer on the result set so that additional results can be loaded asynchronously
            while (rs.hasNext() && cnt < maxResults) {
                rs.next();
                if (columnCount == 1) {
                    res.add(rs.getObject(0));
                } else {
                    res.add(extractResultRow(rs, columnCount));
                }
                cnt++;
            }
            return res;
        }
    }

    private void setTargetOntology(Statement stmt) {
        if (useBackupOntology) {
            stmt.useOntology(Statement.StatementOntology.CENTRAL);
        } else {
            stmt.useOntology(Statement.StatementOntology.TRANSACTIONAL);
        }
    }

    private static Object[] extractResultRow(ResultSet rs, int columnCount) throws OntoDriverException {
        final Object[] row = new Object[columnCount];
        for (int i = 0; i < columnCount; i++) {
            final Object ob = rs.getObject(i);
            row[i] = ob;
        }
        return row;
    }

    @Override
    public Query addContext(URI context) {
        Objects.requireNonNull(context);
        contexts.add(context);
        return this;
    }

    @Override
    public Query addContexts(Collection<URI> contexts) {
        Objects.requireNonNull(contexts);
        this.contexts.addAll(contexts);
        return this;
    }

    @Override
    public Query clearContexts() {
        contexts.clear();
        return this;
    }
}

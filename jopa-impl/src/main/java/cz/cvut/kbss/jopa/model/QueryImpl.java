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
import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.query.QueryHolder;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

public class QueryImpl extends AbstractQuery implements Query {

    private int maxResults;

    public QueryImpl(final QueryHolder query, final ConnectionWrapper connection) {
        super(query, connection);
        this.maxResults = Integer.MAX_VALUE;
    }

    @Override
    public void executeUpdate() {
        executeUpdateImpl();
    }

    @Override
    public List getResultList() {
        try {
            if (maxResults == 0) {
                return Collections.emptyList();
            }
            return getResultListImpl(maxResults);
        } catch (OntoDriverException e) {
            markTransactionForRollback();
            throw queryEvaluationException(e);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
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
            markTransactionForRollback();
            throw queryEvaluationException(e);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
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

    @Override
    public <T> T getParameterValue(Parameter<T> parameter) {
        if (!isBound(parameter)) {
            throw new IllegalStateException("Parameter " + parameter + " is not bound.");
        }
        return (T) query.getParameterValue(parameter);
    }

    @Override
    public Query setParameter(int position, Object value) {
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
        try {
            query.setParameter(query.getParameter(name), value, language);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
        return this;
    }

    @Override
    public <T> Query setParameter(Parameter<T> parameter, T value) {
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
        try {
            query.setParameter(parameter, value, language);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
        return this;
    }

    @Override
    public Query setMaxResults(int maxResults) {
        if (maxResults < 0) {
            markTransactionForRollback();
            throw new IllegalArgumentException("Cannot set maximum number of results to less than 0.");
        }
        this.maxResults = maxResults;
        return this;
    }

    private List<?> getResultListImpl(int maxResults) throws OntoDriverException {
        assert maxResults > 0;

        final Statement stmt = connection.createStatement();
        try {
            setTargetOntology(stmt);
            final ResultSet rs = stmt.executeQuery(query.assembleQuery());
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
        } finally {
            try {
                stmt.close();
            } catch (Exception e) {
                LOG.error("Unable to close statement after query evaluation.", e);
            }
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
}

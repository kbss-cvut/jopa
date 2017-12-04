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

import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.exceptions.NoUniqueResultException;
import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.query.QueryHolder;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class QueryImpl extends AbstractQuery implements Query {

    public QueryImpl(final QueryHolder query, final ConnectionWrapper connection) {
        super(query, connection);
    }

    @Override
    public void executeUpdate() {
        executeUpdateImpl();
    }

    @Override
    public List getResultList() {
        try {
            if (getMaxResults() == 0) {
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

    private List<?> getResultListImpl(int maxResults) throws OntoDriverException {
        assert maxResults > 0;
        final List<Object> res = new ArrayList<>();
        executeQuery(rs -> res.add(extractRow(rs)));
        return res;
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
    public Query setUntypedParameter(int position, Object value) {
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
        try {
            query.setUntypedParameter(query.getParameter(name), value);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
        return this;
    }

    @Override
    public <T> Query setUntypedParameter(Parameter<T> parameter, T value) {
        try {
            query.setUntypedParameter(parameter, value);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
        return this;
    }

    @Override
    public Query setMaxResults(int maxResults) {
        checkNumericParameter(maxResults, "max results");
        this.maxResults = maxResults;
        return this;
    }

    @Override
    public Query setFirstResult(int startPosition) {
        checkNumericParameter(startPosition, "first result offset");
        this.firstResult = startPosition;
        return this;
    }

    Object extractRow(ResultSet resultSet) throws OntoDriverException {
        final int columnCount = resultSet.getColumnCount();
        if (columnCount == 1) {
            return resultSet.getObject(0);
        } else {
            final Object[] row = new Object[columnCount];
            for (int i = 0; i < columnCount; i++) {
                final Object ob = resultSet.getObject(i);
                row[i] = ob;
            }
            return row;
        }
    }
}

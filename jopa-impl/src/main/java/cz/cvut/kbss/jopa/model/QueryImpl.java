/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.query.QueryHolder;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

public class QueryImpl extends AbstractQuery implements Query {

    public QueryImpl(final QueryHolder query, final ConnectionWrapper connection) {
        super(query, connection);
    }

    @Override
    public List getResultList() {
        ensureOpen();
        try {
            if (getMaxResults() == 0) {
                return Collections.emptyList();
            }
            return getResultListImpl();
        } catch (OntoDriverException e) {
            markTransactionForRollback();
            throw queryEvaluationException(e);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
    }

    private List<?> getResultListImpl() throws OntoDriverException {
        final List<Object> res = new ArrayList<>();
        executeQuery(rs -> res.add(extractRow(rs)));
        return res;
    }

    @Override
    public Object getSingleResult() {
        ensureOpen();
        try {
            final List<?> list = getResultListImpl();
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
            if (exceptionCausesRollback(e)) {
                markTransactionForRollback();
            }
            throw e;
        }
    }

    @Override
    public Stream getResultStream() {
        try {
            return executeQueryForStream(this::extractRowAsOptional);
        } catch (OntoDriverException e) {
            markTransactionForRollback();
            throw queryEvaluationException(e);
        } catch (RuntimeException e) {
            if (exceptionCausesRollback(e)) {
                markTransactionForRollback();
            }
            throw e;
        }
    }

    @Override
    public Query setMaxResults(int maxResults) {
        ensureOpen();
        checkNumericParameter(maxResults, "max results");
        query.setMaxResults(maxResults);
        return this;
    }

    @Override
    public Query setFirstResult(int startPosition) {
        ensureOpen();
        checkNumericParameter(startPosition, "first result offset");
        query.setFirstResult(startPosition);
        return this;
    }

    Optional<Object> extractRowAsOptional(ResultRow row) {
        return Optional.of(extractRow(row));
    }

    Object extractRow(ResultRow resultRow) {
        try {
            final int columnCount = resultRow.getColumnCount();
            if (columnCount == 1) {
                return resultRow.getObject(0);
            } else {
                final Object[] row = new Object[columnCount];
                for (int i = 0; i < columnCount; i++) {
                    final Object ob = resultRow.isBound(i) ? resultRow.getObject(i) : null;
                    row[i] = ob;
                }
                return row;
            }
        } catch (OntoDriverException e) {
            throw new OWLPersistenceException(e);
        }
    }
}

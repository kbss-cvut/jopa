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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.exceptions.NoUniqueResultException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.query.QueryHints;
import cz.cvut.kbss.jopa.query.QueryHolder;
import cz.cvut.kbss.jopa.query.sparql.loader.QueryResultLoadingOptimizer;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;

public class TypedQueryImpl<X> extends AbstractQuery implements TypedQuery<X> {

    private final Class<X> resultType;

    private final QueryResultLoadingOptimizer<? extends QueryHolder> queryResultLoadingOptimizer;

    private Descriptor descriptor = new EntityDescriptor();

    public TypedQueryImpl(QueryHolder query, Class<X> resultType, ConnectionWrapper connection,
                          QueryResultLoadingOptimizer<? extends QueryHolder> queryResultLoadingOptimizer) {
        super(query, connection);
        this.resultType = Objects.requireNonNull(resultType);
        this.queryResultLoadingOptimizer = queryResultLoadingOptimizer;
    }

    @Override
    public List<X> getResultList() {
        ensureOpen();
        try {
            return getResultListImpl();
        } catch (OntoDriverException e) {
            markTransactionForRollback();
            throw queryEvaluationException(e);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
    }

    private List<X> getResultListImpl() throws OntoDriverException {
        final List<X> res = new ArrayList<>();
        queryResultLoadingOptimizer.optimizeQueryAssembly(resultType);
        final QueryResultLoader<X> resultLoader = queryResultLoadingOptimizer.getQueryResultLoader(resultType, descriptor);

        executeQuery(rr -> resultLoader.loadResult(rr).ifPresent(res::add));
        resultLoader.loadLastPending().ifPresent(res::add);
        return res;
    }

    public Descriptor getDescriptor() {
        return descriptor;
    }

    @Override
    public X getSingleResult() {
        ensureOpen();
        try {
            final List<X> res = getResultListImpl();
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
            if (exceptionCausesRollback(e)) {
                markTransactionForRollback();
            }
            throw e;
        }
    }

    @Override
    public Stream<X> getResultStream() {
        queryResultLoadingOptimizer.optimizeQueryAssembly(resultType);
        final QueryResultLoader<X> resultLoader = queryResultLoadingOptimizer.getQueryResultLoader(resultType, descriptor);
        try {
            return executeQueryForStream(resultLoader);
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
    public TypedQuery<X> setMaxResults(int maxResults) {
        ensureOpen();
        checkNumericParameter(maxResults, "max results");
        query.setMaxResults(maxResults);
        return this;
    }

    @Override
    public TypedQuery<X> setFirstResult(int startPosition) {
        ensureOpen();
        checkNumericParameter(startPosition, "first result offset");
        query.setFirstResult(startPosition);
        return this;
    }

    @Override
    public TypedQuery<X> setParameter(int position, Object value) {
        super.setParameter(position, value);
        return this;
    }

    @Override
    public TypedQuery<X> setParameter(int position, String value, String language) {
        super.setParameter(position, value, language);
        return this;
    }

    @Override
    public TypedQuery<X> setParameter(String name, Object value) {
        super.setParameter(name, value);
        return this;
    }

    @Override
    public TypedQuery<X> setParameter(String name, String value, String language) {
        super.setParameter(name, value, language);
        return this;
    }

    @Override
    public <T> TypedQuery<X> setParameter(Parameter<T> parameter, T value) {
        super.setParameter(parameter, value);
        return this;
    }

    @Override
    public TypedQuery<X> setParameter(Parameter<String> parameter, String value, String language) {
        super.setParameter(parameter, value, language);
        return this;
    }

    @Override
    public TypedQuery<X> setUntypedParameter(int position, Object value) {
        super.setUntypedParameter(position, value);
        return this;
    }

    @Override
    public TypedQuery<X> setUntypedParameter(String name, Object value) {
        super.setUntypedParameter(name, value);
        return this;
    }

    @Override
    public <T> TypedQuery<X> setUntypedParameter(Parameter<T> parameter, T value) {
        super.setUntypedParameter(parameter, value);
        return this;
    }

    @Override
    public TypedQuery<X> setHint(String hintName, Object value) {
        super.setHint(hintName, value);
        if (QueryHints.ENABLE_ENTITY_LOADING_OPTIMIZER.equals(hintName)) {
            QueryHintsHandler.Hint.getHint(hintName).ifPresent(h -> {
                if ((Boolean) h.getValueToApply(value)) {
                    queryResultLoadingOptimizer.enableOptimization();
                } else {
                    queryResultLoadingOptimizer.disableOptimization();
                }
            });
        }
        return this;
    }

    @Override
    public TypedQuery<X> setDescriptor(Descriptor descriptor) {
        this.descriptor = Objects.requireNonNull(descriptor);
        return this;
    }
}

/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.model.QueryImpl;
import cz.cvut.kbss.jopa.model.ResultSetMappingQuery;
import cz.cvut.kbss.jopa.model.TypedQueryImpl;
import cz.cvut.kbss.jopa.query.QueryParser;
import cz.cvut.kbss.jopa.query.mapper.SparqlResultMapper;
import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;
import cz.cvut.kbss.jopa.query.soql.SoqlQueryParser;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;

import java.util.Objects;

/**
 * Factory for creating SPARQL queries.
 */
public class SparqlQueryFactory {

    private final UnitOfWork uow;
    private final ConnectionWrapper connection;

    private final QueryParser queryParser;
    private final SoqlQueryParser soqlQueryParser;

    public SparqlQueryFactory(UnitOfWork uow, ConnectionWrapper connection) {
        assert uow != null;
        assert connection != null;
        this.uow = uow;
        this.connection = connection;
        this.queryParser = new SparqlQueryParser(new ParameterValueFactory(uow));
        this.soqlQueryParser = new SoqlQueryParser(queryParser, uow.getMetamodel());
    }

    /**
     * Creates query object representing a native SPARQL query.
     *
     * @param sparql The query
     * @return Query object
     * @throws NullPointerException If {@code sparql} is {@code null}
     */
    public QueryImpl createNativeQuery(String sparql) {
        Objects.requireNonNull(sparql);

        return new QueryImpl(queryParser.parseQuery(sparql), connection);
    }

    /**
     * Creates typed query object representing a native SPARQL query.
     *
     * @param sparql      The query
     * @param resultClass Type of the results
     * @return Query object
     * @throws NullPointerException If {@code sparql} or {@code resultClass} is {@code null}
     */
    public <T> TypedQueryImpl<T> createNativeQuery(String sparql, Class<T> resultClass) {
        Objects.requireNonNull(sparql);

        return createQueryImpl(sparql, resultClass, queryParser);
    }

    private <T> TypedQueryImpl<T> createQueryImpl(String query, Class<T> resultClass, QueryParser parser) {
        Objects.requireNonNull(resultClass);

        return new TypedQueryImpl<>(parser.parseQuery(query), resultClass, connection, uow);
    }

    /**
     * Creates a query object representing a native SPARQL query.
     *
     * @param sparql           The query
     * @param resultSetMapping Name of the result set mapping to apply
     * @return Query object * @throws NullPointerException If {@code sparql} or {@code resultSetMapping} is {@code null}
     */
    public QueryImpl createNativeQuery(String sparql, String resultSetMapping) {
        Objects.requireNonNull(sparql);
        Objects.requireNonNull(resultSetMapping);

        final SparqlResultMapper mapper = uow.getResultSetMappingManager().getMapper(resultSetMapping);
        return new ResultSetMappingQuery(queryParser.parseQuery(sparql), connection, mapper, uow);
    }

    /**
     * Creates query object representing a native SPARQL query.
     *
     * @param query The query
     * @return Query object
     * @throws NullPointerException If {@code sparql} is {@code null}
     */
    public QueryImpl createQuery(String query) {
        Objects.requireNonNull(query);

        return new QueryImpl(soqlQueryParser.parseQuery(query), connection);
    }

    /**
     * Creates typed query object representing a native SPARQL query.
     *
     * @param query       The query
     * @param resultClass Type of the results param URI of the ontology context against which the query will be
     *                    evaluated
     * @return Query object
     * @throws NullPointerException If {@code sparql} or {@code resultClass} is {@code null}
     */
    public <T> TypedQueryImpl<T> createQuery(String query, Class<T> resultClass) {
        Objects.requireNonNull(query);
        return createQueryImpl(query, resultClass, soqlQueryParser);
    }

    /**
     * Creates a query object representing a native SPARQL query.
     *
     * @param name The name of the query defined in metadata
     * @return Query object
     * @throws IllegalArgumentException If a query has not been defined with the given name
     */
    public QueryImpl createNamedQuery(String name) {
        final String query = uow.getNamedQueryManager().getQuery(name);
        return createNativeQuery(query);
    }

    /**
     * Creates a typed query object representing a native SPARQL query.
     *
     * @param name        The name of the query defined in metadata
     * @param resultClass Type of the results param URI of the ontology context against which the query will be
     *                    evaluated
     * @return Query object
     * @throws IllegalArgumentException If a query has not been defined with the given name
     */
    public <T> TypedQueryImpl<T> createNamedQuery(String name, Class<T> resultClass) {
        final String query = uow.getNamedQueryManager().getQuery(name);
        return createNativeQuery(query, resultClass);
    }
}

/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.model.QueryImpl;
import cz.cvut.kbss.jopa.model.ResultSetMappingQuery;
import cz.cvut.kbss.jopa.model.TypedQueryImpl;
import cz.cvut.kbss.jopa.query.QueryParser;
import cz.cvut.kbss.jopa.query.mapper.SparqlResultMapper;
import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;
import cz.cvut.kbss.jopa.query.soql.SoqlQueryParser;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.jopa.sessions.QueryFactory;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.jopa.utils.ErrorUtils;

import java.util.Objects;

public class SparqlQueryFactory implements QueryFactory {

    private final UnitOfWorkImpl uow;
    private final ConnectionWrapper connection;

    private final QueryParser queryParser;
    private final SoqlQueryParser soqlQueryParser;

    public SparqlQueryFactory(UnitOfWorkImpl uow, ConnectionWrapper connection) {
        assert uow != null;
        assert connection != null;
        this.uow = uow;
        this.connection = connection;
        this.queryParser = new SparqlQueryParser(new ParameterValueFactory(uow));
        this.soqlQueryParser = new SoqlQueryParser(queryParser, uow.getMetamodel());
    }

    @Override
    public QueryImpl createNativeQuery(String sparql) {
        Objects.requireNonNull(sparql);

        return new QueryImpl(queryParser.parseQuery(sparql), connection);
    }

    @Override
    public <T> TypedQueryImpl<T> createNativeQuery(String sparql, Class<T> resultClass) {
        Objects.requireNonNull(sparql, ErrorUtils.getNPXMessageSupplier("sparql"));

        return createQueryImpl(sparql, resultClass, queryParser);
    }

    private <T> TypedQueryImpl<T> createQueryImpl(String query, Class<T> resultClass, QueryParser parser) {
        Objects.requireNonNull(resultClass, ErrorUtils.getNPXMessageSupplier("resultClass"));

        final TypedQueryImpl<T> tq = new TypedQueryImpl<>(parser.parseQuery(query), resultClass, connection, uow);
        tq.setUnitOfWork(uow);
        return tq;
    }

    @Override
    public QueryImpl createNativeQuery(String sparql, String resultSetMapping) {
        Objects.requireNonNull(sparql, ErrorUtils.getNPXMessageSupplier("sparql"));
        Objects.requireNonNull(resultSetMapping, ErrorUtils.getNPXMessageSupplier("resultSetMapping"));

        final SparqlResultMapper mapper = uow.getResultSetMappingManager().getMapper(resultSetMapping);
        return new ResultSetMappingQuery(queryParser.parseQuery(sparql), connection, mapper, uow);
    }

    @Override
    public QueryImpl createQuery(String query) {
        Objects.requireNonNull(query);

        return new QueryImpl(soqlQueryParser.parseQuery(query), connection);
    }

    @Override
    public <T> TypedQueryImpl<T> createQuery(String query, Class<T> resultClass) {
        Objects.requireNonNull(query, ErrorUtils.getNPXMessageSupplier("query"));
        return createQueryImpl(query, resultClass, soqlQueryParser);
    }

    @Override
    public QueryImpl createNamedQuery(String name) {
        final String query = uow.getNamedQueryManager().getQuery(name);
        return createNativeQuery(query);
    }

    @Override
    public <T> TypedQueryImpl<T> createNamedQuery(String name, Class<T> resultClass) {
        final String query = uow.getNamedQueryManager().getQuery(name);
        return createNativeQuery(query, resultClass);
    }
}

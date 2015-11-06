package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.owlapi.QueryImpl;
import cz.cvut.kbss.jopa.owlapi.TypedQueryImpl;
import cz.cvut.kbss.jopa.query.QueryParser;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.jopa.sessions.QueryFactory;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.jopa.utils.ErrorUtils;

import java.util.List;
import java.util.Objects;

public class SparqlQueryFactory implements QueryFactory {

    private final UnitOfWorkImpl uow;
    private final ConnectionWrapper connection;

    private final QueryParser queryParser;

    public SparqlQueryFactory(UnitOfWorkImpl uow, ConnectionWrapper connection) {
        assert uow != null;
        assert connection != null;
        this.uow = uow;
        this.connection = connection;
        this.queryParser = new SparqlQueryParser();
    }

    @Override
    public Query<List<String>> createNativeQuery(String sparql) {
        Objects.requireNonNull(sparql, ErrorUtils.constructNPXMessage("sparql"));

        final QueryImpl q = new QueryImpl(queryParser.parseQuery(sparql), connection);
        q.setUseBackupOntology(uow.useBackupOntologyForQueryProcessing());
        return q;
    }

    @Override
    public <T> TypedQuery<T> createNativeQuery(String sparql, Class<T> resultClass) {
        Objects.requireNonNull(sparql, ErrorUtils.constructNPXMessage("sparql"));
        Objects.requireNonNull(resultClass, ErrorUtils.constructNPXMessage("resultClass"));

        final TypedQueryImpl<T> tq = new TypedQueryImpl<>(queryParser.parseQuery(sparql), resultClass, connection, uow);
        tq.setUnitOfWork(uow);
        tq.setUseBackupOntology(uow.useBackupOntologyForQueryProcessing());
        return tq;
    }

    @Override
    public Query createQuery(String query) {
        Objects.requireNonNull(query, ErrorUtils.constructNPXMessage("query"));

        // We do not support any more abstract syntax, yet
        return createNativeQuery(query);
    }

    @Override
    public <T> TypedQuery<T> createQuery(String query, Class<T> resultClass) {
        Objects.requireNonNull(query, ErrorUtils.constructNPXMessage("query"));
        Objects.requireNonNull(resultClass, ErrorUtils.constructNPXMessage("resultClass"));

        // We do not support any more abstract syntax, yet
        return createNativeQuery(query, resultClass);
    }
}

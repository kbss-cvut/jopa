package cz.cvut.kbss.ontodriver.sesame.connector;


import cz.cvut.kbss.ontodriver.sesame.config.SesameOntoDriverProperties;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import org.openrdf.query.*;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.RepositoryException;

/**
 * Actual implementation of statement processing.
 */
class ConnectionStatementExecutor implements StatementExecutor {

    private final RepositoryConnection connection;

    ConnectionStatementExecutor(RepositoryConnection connection) {
        this.connection = connection;
    }

    @Override
    public TupleQueryResult executeSelectQuery(String query) throws SesameDriverException {
        try {
            final TupleQuery tq = connection.prepareTupleQuery(QueryLanguage.SPARQL, query);
            return new QueryResult(tq.evaluate(), connection);
        } catch (MalformedQueryException | QueryEvaluationException | RepositoryException e) {
            throw new SesameDriverException(e);
        }
    }

    @Override
    public boolean executeBooleanQuery(String query) throws SesameDriverException {
        try {
            return connection.prepareBooleanQuery(QueryLanguage.SPARQL, query).evaluate();
        } catch (MalformedQueryException | QueryEvaluationException | RepositoryException e) {
            throw new SesameDriverException(e);
        }
    }

    @Override
    public void executeUpdate(String query) throws SesameDriverException {
        try {
            final Update u = connection.prepareUpdate(QueryLanguage.SPARQL, query);
            u.execute();
        } catch (MalformedQueryException | UpdateExecutionException | RepositoryException e) {
            throw new SesameDriverException(e);
        }
    }
}

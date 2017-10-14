package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.query.QueryHolder;
import cz.cvut.kbss.jopa.query.mapper.SparqlResultMapper;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

/**
 * Implementation of query using result set mapping configuration.
 */
public class ResultSetMappingQuery extends QueryImpl {

    private final SparqlResultMapper mapper;

    public ResultSetMappingQuery(QueryHolder query, ConnectionWrapper connection, SparqlResultMapper mapper) {
        super(query, connection);
        this.mapper = mapper;
    }

    @Override
    Object extractRow(ResultSet resultSet) throws OntoDriverException {
        return mapper.map(resultSet);
    }
}

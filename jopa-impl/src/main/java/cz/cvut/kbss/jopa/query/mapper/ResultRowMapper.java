package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.ontodriver.ResultSet;

/**
 * Represents a single {@link cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping} instance.
 * <p>
 * This instance can contain multiple {@link SparqlResultMapper} instances, representing the individual mappings specified by
 * the {@link cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping}.
 */
public class ResultRowMapper implements SparqlResultMapper {

    @Override
    public Object map(ResultSet resultSet) {
        return null;
    }
}

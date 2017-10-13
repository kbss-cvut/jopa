package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.ontodriver.ResultSet;

/**
 * Maps SPARQL result set value (a subset of the current row) to output based on a {@link
 * cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping} configuration.
 */
public interface SparqlResultMapper {

    /**
     * Maps (a subset of) the current row of the specified result set to output based on this instance's configuration.
     *
     * @param resultSet The result set to map
     * @return Result of the mapping
     */
    Object map(ResultSet resultSet);
}

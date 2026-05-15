package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;
import cz.cvut.kbss.ontodriver.model.Value;

import java.util.stream.Stream;

/**
 * Supports modifying a query to project GROUP_CONCAT for a given attribute.
 * <p>
 * Both generating the GROUP_CONCAT projection and reading the values from the result row are supported.
 */
abstract class GroupConcatQueryModifier {

    static final String GROUP_CONCAT_SEPARATOR = "\u001F";
    static final String GROUP_CONCAT_SUFFIX = "_gc";

    final QueryVariableMapping variableMapping;

    boolean triggered;

    GroupConcatQueryModifier(QueryVariableMapping variableMapping) {
        this.variableMapping = variableMapping;
    }

    /**
     * Generates a {@literal GROUP_CONCAT} pattern for query projection.
     *
     * @return Pattern for query projection
     */
    abstract String generateGroupConcat();

    /**
     * Reads and extracts values created by a {@literal GROUP_CONCAT} pattern.
     *
     * @param row Row to extract values from
     * @return Extracted values
     * @throws OntoDriverException If reading the values fails
     */
    abstract Stream<Value<?>> readValue(ResultRow row) throws OntoDriverException;
}

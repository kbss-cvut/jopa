package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.annotations.VariableResult;
import cz.cvut.kbss.jopa.utils.DatatypeTransformer;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

/**
 * Maps SPARQL query result to target value based on a {@link VariableResult} configuration.
 */
public class VariableResultMapper implements SparqlResultMapper {

    private final String name;
    private final Class<?> targetType;

    public VariableResultMapper(VariableResult mapping) {
        this.name = mapping.name();
        this.targetType = mapping.type();
    }

    /**
     * Maps value from the current line of the specified result set according to the {@link VariableResult}
     * configuration represented by this instance.
     *
     * @param resultSet Query result set to read
     * @return The mapped value
     */
    @Override
    public Object map(ResultSet resultSet) {
        try {
            final Object value = resultSet.getObject(name);
            if (!void.class.equals(targetType)) {
                return DatatypeTransformer.transform(value, targetType);
            }
            return value;
        } catch (OntoDriverException e) {
            throw new OWLPersistenceException(e);
        }
    }
}

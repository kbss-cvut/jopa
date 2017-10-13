package cz.cvut.kbss.jopa.query;

import cz.cvut.kbss.jopa.query.mapper.SparqlResultMapper;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * Manages result set mappers, which are used to transform query result sets to output based on {@link
 * cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping}.
 */
public class ResultSetMappingManager {

    private final Map<String, SparqlResultMapper> mappers = new HashMap<>();

    /**
     * Adds the specified mapper to this manager, so that it can be used later.
     *
     * @param mappingName Named of the mapping for which the mapper is added
     * @param mapper      The mapper to register
     * @throws IllegalArgumentException If a mapping with the same name already exists
     */
    public void addMapper(String mappingName, SparqlResultMapper mapper) {
        Objects.requireNonNull(mappingName);
        Objects.requireNonNull(mapper);
        if (mappers.containsKey(mappingName)) {
            throw new IllegalArgumentException("Mapping " + mappingName + " already exists in this persistence unit.");
        }
        mappers.put(mappingName, mapper);
    }

    /**
     * Gets mapper for the specified mapping name.
     *
     * @param mappingName Name of the mapping
     * @return Matching mapper
     * @throws IllegalArgumentException If there is no mapper for such mapping
     */
    public SparqlResultMapper getMapper(String mappingName) {
        if (!mappers.containsKey(mappingName)) {
            throw new IllegalArgumentException("Mapping " + mappingName + " not found in this persistence unit.");
        }
        return mappers.get(mappingName);
    }
}

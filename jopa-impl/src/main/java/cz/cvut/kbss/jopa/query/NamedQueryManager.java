package cz.cvut.kbss.jopa.query;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * Manages named queries in the persistence unit.
 */
public class NamedQueryManager {

    private final Map<String, String> queryMap = new HashMap<>();

    /**
     * Adds a named query mapping.
     *
     * @param name  Named of the query
     * @param query Query string
     * @throws IllegalArgumentException If there already exists a mapping for the specified name
     */
    public void addNamedQuery(String name, String query) {
        Objects.requireNonNull(name);
        Objects.requireNonNull(query);
        if (queryMap.containsKey(name)) {
            throw new IllegalArgumentException("Query with name " + name + " already exists in this persistence unit.");
        }
        queryMap.put(name, query);
    }

    /**
     * Gets a query mapped by the specified name.
     *
     * @param name Query name
     * @return Query string
     * @throws IllegalArgumentException If a query has not been defined with the given name
     */
    public String getQuery(String name) {
        if (!queryMap.containsKey(name)) {
            throw new IllegalArgumentException("Query with name " + name + " was not found in this persistence unit.");
        }
        return queryMap.get(name);
    }
}

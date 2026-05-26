package cz.cvut.kbss.jopa.model;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * Manages named {@link EntityGraph}s, which are used to define fetching strategies for entities.
 * <p>
 * This class is not synchronized because it is expected that modifications are done serially when building the
 * persistence unit and all further interactions are read-only.
 */
public class NamedEntityGraphManager {

    private final Map<String, EntityGraph<?>> namedEntityGraphs = new HashMap<>();

    /**
     * Registers the specified {@link EntityGraph} under the specified name.
     *
     * @param graphName   Name of the named entity graph
     * @param entityGraph Entity graph to register
     * @throws IllegalArgumentException If a named entity graph with the same name already exists
     */
    public void addEntityGraph(String graphName, EntityGraph<?> entityGraph) {
        Objects.requireNonNull(graphName);
        Objects.requireNonNull(entityGraph);
        if (namedEntityGraphs.containsKey(graphName)) {
            throw new IllegalArgumentException("Entity graph " + graphName + " already exists in this persistence unit.");
        }
        namedEntityGraphs.put(graphName, entityGraph);
    }

    /**
     * Gets an {@link EntityGraph} with the specified name.
     *
     * @param graphName Name of entity graph to retrieve
     * @return Matching entity graph
     * @throws IllegalArgumentException If no such entity graph is found
     */
    public EntityGraph<?> getEntityGraph(String graphName) {
        if (!namedEntityGraphs.containsKey(graphName)) {
            throw new IllegalArgumentException("Entity graph " + graphName + " not found in this persistence unit.");
        }
        return namedEntityGraphs.get(graphName);
    }

    /**
     * Checks whether this manager contains a graph with the specified name
     *
     * @param graphName Name to search for
     * @return {@code true if such graph is registered here, {@code false} otherwise
     */
    public boolean hasEntityGraph(String graphName) {
        return namedEntityGraphs.containsKey(graphName);
    }
}

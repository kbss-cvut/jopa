/**
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
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

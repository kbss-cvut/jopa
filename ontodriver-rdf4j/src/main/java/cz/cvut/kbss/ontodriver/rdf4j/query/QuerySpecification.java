/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.ontodriver.rdf4j.query;

import java.util.Objects;

/**
 * Represents a query and configuration relevant to the query execution.
 */
public class QuerySpecification {

    private final String query;

    private boolean includeInference = true;

    private QuerySpecification(String query) {
        this.query = Objects.requireNonNull(query);
    }

    public String getQuery() {
        return query;
    }

    public QuerySpecification includeInference(boolean includeInference) {
        this.includeInference = includeInference;
        return this;
    }

    public boolean isIncludeInference() {
        return includeInference;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        QuerySpecification that = (QuerySpecification) o;
        return includeInference == that.includeInference && query.equals(that.query);
    }

    @Override
    public int hashCode() {
        return Objects.hash(query, includeInference);
    }

    public static QuerySpecification query(String query) {
        return new QuerySpecification(query);
    }
}

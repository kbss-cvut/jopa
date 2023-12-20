/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.Statement;

import java.util.Objects;

class QuerySpecification {

    private final String query;

    private Statement statement;

    private boolean disableInference = false;

    private QuerySpecification(String query) {
        this.query = query;
    }

    public String getQuery() {
        return query;
    }

    public Statement getStatement() {
        return statement;
    }

    public boolean isDisableInference() {
        return disableInference;
    }

    public QuerySpecification statement(Statement statement) {
        this.statement = statement;
        return this;
    }

    public QuerySpecification disableInference(boolean disableInference) {
        this.disableInference = disableInference;
        return this;
    }

    public static QuerySpecification query(String query) {
        return new QuerySpecification(query);
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
        return disableInference == that.disableInference && query.equals(that.query);
    }

    @Override
    public int hashCode() {
        return Objects.hash(query, disableInference);
    }
}

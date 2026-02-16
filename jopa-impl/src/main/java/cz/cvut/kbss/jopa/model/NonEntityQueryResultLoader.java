/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;

import java.util.Optional;

/**
 * Query result loader for a non-entity result type.
 * <p>
 * The loader takes the result row and attempts to load the first binding as an instance of the specified class.
 *
 * @param <T> Result type
 */
public class NonEntityQueryResultLoader<T> implements QueryResultLoader<T> {

    private final Class<T> resultType;

    public NonEntityQueryResultLoader(Class<T> resultType) {
        this.resultType = resultType;
    }

    @Override
    public Optional<T> loadResult(ResultRow resultRow) {
        try {
            return Optional.of(resultRow.getObject(0, resultType));
        } catch (OntoDriverException e) {
            throw new OWLPersistenceException("Unable to map the query result to class " + resultType, e);
        }
    }
}

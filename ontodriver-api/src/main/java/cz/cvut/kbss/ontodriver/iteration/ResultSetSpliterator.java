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
package cz.cvut.kbss.ontodriver.iteration;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.exception.OntoDriverRuntimeException;

import java.util.Objects;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.function.Consumer;

/**
 * {@link Spliterator} implementation for a {@link ResultSet}.
 * <p>
 * Note that the methods wrap {@link OntoDriverException}s possibly thrown by the underlying result set in a {@link OntoDriverRuntimeException}
 * in order to support the {@link Spliterator} API.
 */
public class ResultSetSpliterator extends Spliterators.AbstractSpliterator<ResultRow> {

    private final ResultSet resultSet;

    /**
     * Creates a spliterator reporting unknown estimate size and the following characteristics:
     * <ul>
     * <li>{@link Spliterator#IMMUTABLE}</li>
     * <li>{@link Spliterator#ORDERED}</li>
     * <li>{@link Spliterator#NONNULL}</li>
     * </ul>
     *
     * @param resultSet {@code ResultSet} on which the iteration will be performed
     */
    public ResultSetSpliterator(ResultSet resultSet) {
        super(Long.MAX_VALUE, Spliterator.IMMUTABLE | Spliterator.ORDERED | Spliterator.NONNULL);
        this.resultSet = Objects.requireNonNull(resultSet);
    }

    @Override
    public boolean tryAdvance(Consumer<? super ResultRow> action) {
        Objects.requireNonNull(action);
        try {
            if (resultSet.hasNext()) {
                resultSet.next();
                action.accept(new DelegatingResultRow(resultSet));
                return true;
            } else {
                return false;
            }
        } catch (OntoDriverException e) {
            throw new OntoDriverRuntimeException(e);
        }
    }

    @Override
    public void forEachRemaining(Consumer<? super ResultRow> action) {
        Objects.requireNonNull(action);
        try {
            // Row is just a view on the result set which does the actual iteration
            final DelegatingResultRow row = new DelegatingResultRow(resultSet);
            while (resultSet.hasNext()) {
                resultSet.next();
                action.accept(row);
            }
        } catch (OntoDriverException e) {
            throw new OntoDriverRuntimeException(e);
        }
    }
}

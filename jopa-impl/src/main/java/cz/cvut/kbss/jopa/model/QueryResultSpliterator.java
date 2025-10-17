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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.ontodriver.iteration.ResultRow;

import java.util.Spliterator;
import java.util.Spliterators;
import java.util.function.Consumer;

/**
 * Spliterator for processing {@link cz.cvut.kbss.ontodriver.ResultSet} from {@link cz.cvut.kbss.jopa.model.query.Query}
 * or {@link cz.cvut.kbss.jopa.model.query.TypedQuery} stream support.
 * <p>
 * The main responsibilities of this spliterator are extracting result rows using the specified mapper, passing the
 * extraction result to the specified consumer and invoking the {@code onClose} handler once the iteration is finished.
 * This handler releases the underlying statement and result set.
 *
 * @param <X> The type of the extracted item
 */
class QueryResultSpliterator<X> extends Spliterators.AbstractSpliterator<X> {

    private final Spliterator<ResultRow> resultSetSpliterator;
    private final QueryResultLoader<X> resultLoader;
    private final Runnable onClose;

    QueryResultSpliterator(Spliterator<ResultRow> resultSetSpliterator, QueryResultLoader<X> resultLoader,
                           Runnable onClose) {
        super(Long.MAX_VALUE, Spliterator.IMMUTABLE | Spliterator.ORDERED | Spliterator.NONNULL);
        this.resultSetSpliterator = resultSetSpliterator;
        this.resultLoader = resultLoader;
        this.onClose = onClose;
    }

    private void mapAndApply(ResultRow row, Consumer<? super X> action) {
        resultLoader.loadResult(row).ifPresent(action);
    }

    @Override
    public boolean tryAdvance(Consumer<? super X> action) {
        try {
            final boolean result = resultSetSpliterator.tryAdvance(row -> mapAndApply(row, action));
            if (!result) {
                resultLoader.loadLastPending().ifPresent(action);
                onClose.run();
            }
            return result;
        } catch (RuntimeException e) {
            onClose.run();
            throw e;
        }
    }

    @Override
    public void forEachRemaining(Consumer<? super X> action) {
        try {
            resultSetSpliterator.forEachRemaining(row -> mapAndApply(row, action));
        } finally {
            resultLoader.loadLastPending().ifPresent(action);
            onClose.run();
        }
    }
}

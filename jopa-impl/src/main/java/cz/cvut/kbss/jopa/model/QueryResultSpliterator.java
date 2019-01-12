package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.utils.Procedure;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;

import java.util.Optional;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.function.Consumer;
import java.util.function.Function;

class QueryResultSpliterator<X> extends Spliterators.AbstractSpliterator<X> {

    private final Spliterator<ResultRow> resultSetSpliterator;
    private final Function<ResultRow, Optional<X>> mapper;
    private final Procedure onClose;

    QueryResultSpliterator(Spliterator<ResultRow> resultSetSpliterator, Function<ResultRow, Optional<X>> mapper,
                           Procedure onClose) {
        super(Long.MAX_VALUE, Spliterator.IMMUTABLE | Spliterator.ORDERED | Spliterator.NONNULL);
        this.resultSetSpliterator = resultSetSpliterator;
        this.mapper = mapper;
        this.onClose = onClose;
    }

    private void mapAndApply(ResultRow row, Consumer<? super X> action) {
        mapper.apply(row).ifPresent(action);
    }

    @Override
    public boolean tryAdvance(Consumer<? super X> action) {
        return resultSetSpliterator.tryAdvance((row) -> mapAndApply(row, action));
    }

    @Override
    public void forEachRemaining(Consumer<? super X> action) {
        try {
            resultSetSpliterator.forEachRemaining((row) -> mapAndApply(row, action));
        } finally {
            onClose.execute();
        }
    }
}

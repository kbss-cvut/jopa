package cz.cvut.kbss.ontodriver.iteration;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Iterator over a {@link ResultSet}.
 */
public class ResultSetIterator implements Iterator<ResultRow> {

    private final ResultSet resultSet;

    public ResultSetIterator(ResultSet resultSet) {
        this.resultSet = resultSet;
    }

    @Override
    public boolean hasNext() {
        try {
            return resultSet.hasNext();
        } catch (OntoDriverException e) {
            throw new ResultSetIterationException(e);
        }
    }

    @Override
    public ResultRow next() {
        if (!hasNext()) {
            throw new NoSuchElementException();
        }
        try {
            resultSet.next();
            return new DelegatingResultRow(resultSet);
        } catch (OntoDriverException e) {
            throw new ResultSetIterationException(e);
        }
    }
}

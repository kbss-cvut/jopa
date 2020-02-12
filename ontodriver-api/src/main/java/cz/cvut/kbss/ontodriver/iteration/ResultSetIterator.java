/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.iteration;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.exception.OntoDriverRuntimeException;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Iterator over a {@link ResultSet}.
 * <p>
 * Note that the methods wrap {@link OntoDriverException}s possibly thrown by the underlying result set in a {@link OntoDriverRuntimeException}
 * in order to support the {@link Iterator} API.
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

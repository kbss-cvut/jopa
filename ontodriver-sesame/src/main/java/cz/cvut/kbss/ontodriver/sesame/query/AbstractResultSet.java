package cz.cvut.kbss.ontodriver.sesame.query;

import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver_new.ResultSet;

import java.util.NoSuchElementException;
import java.util.Observer;

/**
 * @author kidney
 */
public abstract class AbstractResultSet implements ResultSet {

    private final Statement statement;

    int index;
    private boolean open;

    AbstractResultSet(Statement statement) {
        assert statement != null;

        this.statement = statement;
        this.index = -1;
        this.open = true;
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    @Override
    public int getRowIndex() throws OntoDriverException {
        ensureOpen();
        return index;
    }

    @Override
    public Statement getStatement() throws OntoDriverException {
        ensureOpen();
        return statement;
    }

    @Override
    public boolean isFirst() throws OntoDriverException {
        ensureOpen();
        return index == 0;
    }

    protected void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("The result set is closed!");
        }
    }

    @Override
    public void close() throws OntoDriverException {
        this.open = false;
    }

    @Override
    public void first() throws OntoDriverException {
        throw new UnsupportedOperationException(
                "Returning to the first row is not supported by this result set.");
    }

    @Override
    public void previous() throws OntoDriverException {
        throw new UnsupportedOperationException("Going back is not supported by this result set.");
    }

    @Override
    public void registerObserver(Observer observer) throws OntoDriverException {
        // TODO Auto-generated method stub

    }

    @Override
    public void relative(int rows) throws OntoDriverException {
        setRowIndex(index + rows);
    }

    @Override
    public void last() throws OntoDriverException {
        ensureOpen();
        while (hasNext()) {
            next();
        }
    }

    @Override
    public void next() throws OntoDriverException {
        ensureOpen();
        if (!hasNext()) {
            throw new NoSuchElementException();
        }
        index++;
    }

    @Override
	public void setRowIndex(int rowIndex) throws OntoDriverException {
		ensureOpen();
		if (rowIndex < index) {
			throw new UnsupportedOperationException(
					"Going back in this result set is not supported.");
		}
		if (rowIndex == index) {
			return;
		}
		while (index <= rowIndex) {
			next();
		}
	}
}

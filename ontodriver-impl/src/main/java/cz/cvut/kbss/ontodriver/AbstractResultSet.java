package cz.cvut.kbss.ontodriver;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public abstract class AbstractResultSet implements ResultSet {

	private final Statement statement;

	protected int index;
	protected boolean open;

	protected AbstractResultSet(Statement statement) {
		assert statement != null;
		this.statement = statement;
		this.open = true;
		this.index = -1;
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	@Override
	public void close() throws OntoDriverException {
		this.open = false;
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

	@Override
	public void next() throws OntoDriverException {
		ensureOpen();
		index++;
	}

	protected void ensureOpen() {
		if (!open) {
			throw new IllegalStateException("The result set is closed!");
		}
	}
}

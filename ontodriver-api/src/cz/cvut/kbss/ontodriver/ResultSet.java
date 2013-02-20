package cz.cvut.kbss.ontodriver;

import java.util.Observer;

/**
 * Represents a set of results of a SPARQL query. </p>
 * 
 * This interface declares methods for getting values from a set of results of a
 * SPARQL query issued to an ontology.
 * 
 * @author kidney
 * 
 */
public interface ResultSet extends Closeable {

	/**
	 * {@inheritDoc} </p>
	 * 
	 * Calling this method also results in immediate disconnection of all
	 * registered observers and cancellation of any running reasoning associated
	 * with this result set.
	 */
	public void close() throws OntoDriverException;

	/**
	 * Retrieves index of a column with the specified label.
	 * 
	 * @param columnLabel
	 *            Label of the column
	 * @return index of the column
	 * @throws OntoDriverException
	 *             If there is no column with the specified label, this method
	 *             is called on closed result set or there occurs some other
	 *             error
	 */
	public int findColumn(String columnLabel) throws OntoDriverException;

	/**
	 * Move the cursor to the first row.
	 * 
	 * @throws OntoDriverException
	 *             If this method is called on closed result set or some other
	 *             error occurs
	 */
	public void first() throws OntoDriverException;

	/**
	 * Retrieves value from column at the specified index and returns it as a
	 * {@code boolean}.
	 * 
	 * @param columnIndex
	 *            Column index, the first column has index 0
	 * @return {@code boolean} value
	 * @throws OntoDriverException
	 *             If the {@code columnIndex} is not a valid column index, the
	 *             value cannot be cast to {@code boolean}, this method is
	 *             called on closed result set or there occurs some other error
	 */
	public boolean getBoolean(int columnIndex) throws OntoDriverException;

	/**
	 * Retrieves value from column with the specified label and returns it as a
	 * {@code boolean}.
	 * 
	 * @param columnLabel
	 *            Label of the column
	 * @return {@code boolean} value
	 * @throws OntoDriverException
	 *             If there is no column with the specified label, the value
	 *             cannot be cast to {@code boolean}, this method is called on
	 *             closed result set or there occurs some other error
	 */
	public boolean getBoolean(String columnLabel) throws OntoDriverException;

	/**
	 * Retrieves value from column at the specified index and returns it as
	 * {@code byte}.
	 * 
	 * @param columnIndex
	 *            Column index, the first column has index 0
	 * @return {@code byte} value
	 * @throws OntoDriverException
	 *             If the {@code columnIndex} is not a valid column index, the
	 *             value cannot be cast to {@code byte}, this method is called
	 *             on closed result set or there occurs some other error
	 */
	public byte getByte(int columnIndex) throws OntoDriverException;

	/**
	 * Retrieves value from column with the specified label and returns it as
	 * {@code byte}.
	 * 
	 * @param columnLabel
	 *            Label of the column
	 * @return {@code byte} value
	 * @throws OntoDriverException
	 *             If there is no column with the specified label, the value
	 *             cannot be cast to {@code byte}, this method is called on
	 *             closed result set or there occurs some other error
	 */
	public byte getByte(String columnLabel) throws OntoDriverException;

	/**
	 * Retrieves value from column at the specified index and returns it as
	 * {@code double}.
	 * 
	 * @param columnIndex
	 *            Column index, the first column has index 0
	 * @return {@code double} value
	 * @throws OntoDriverException
	 *             If the {@code columnIndex} is not a valid column index, the
	 *             value cannot be cast to {@code double}, this method is called
	 *             on closed result set or there occurs some other error
	 */
	public double getDouble(int columnIndex) throws OntoDriverException;

	/**
	 * Retrieves value from column with the specified label and returns it as
	 * {@code double}.
	 * 
	 * @param columnLabel
	 *            Label of the column
	 * @return {@code double} value
	 * @throws OntoDriverException
	 *             If there is no column with the specified label, the value
	 *             cannot be cast to {@code double}, this method is called on
	 *             closed result set or there occurs some other error
	 */
	public double getDouble(String columnLabel) throws OntoDriverException;

	/**
	 * Retrieves value from column at the specified index and returns it as
	 * {@code float}.
	 * 
	 * @param columnIndex
	 *            Column index, the first column has index 0
	 * @return {@code float} value
	 * @throws OntoDriverException
	 *             If the {@code columnIndex} is not a valid column index, the
	 *             value cannot be cast to {@code float}, this method is called
	 *             on closed result set or there occurs some other error
	 */
	public float getFloat(int columnIndex) throws OntoDriverException;

	/**
	 * Retrieves value from column with the specified label and returns it as
	 * {@code float}.
	 * 
	 * @param columnLabel
	 *            Label of the column
	 * @return {@code float} value
	 * @throws OntoDriverException
	 *             If there is no column with the specified label, the value
	 *             cannot be cast to {@code float}, this method is called on
	 *             closed result set or there occurs some other error
	 */
	public float getFloat(String columnLabel) throws OntoDriverException;

	/**
	 * Retrieves value from column at the specified index and returns it as
	 * {@code int}.
	 * 
	 * @param columnIndex
	 *            Column index, the first column has index 0
	 * @return {@code int} value
	 * @throws OntoDriverException
	 *             If the {@code columnIndex} is not a valid column index, the
	 *             value cannot be cast to {@code int}, this method is called on
	 *             closed result set or there occurs some other error
	 */
	public int getInt(int columnIndex) throws OntoDriverException;

	/**
	 * Retrieves value from column with the specified label and returns it as
	 * {@code int}.
	 * 
	 * @param columnLabel
	 *            Label of the column
	 * @return {@code int} value
	 * @throws OntoDriverException
	 *             If there is no column with the specified label, the value
	 *             cannot be cast to {@code int}, this method is called on
	 *             closed result set or there occurs some other error
	 */
	public int getInt(String columnLabel) throws OntoDriverException;

	/**
	 * Retrieves value from column at the specified index and returns it as
	 * {@code long}.
	 * 
	 * @param columnIndex
	 *            Column index, the first column has index 0
	 * @return {@code long} value
	 * @throws OntoDriverException
	 *             If the {@code columnIndex} is not a valid column index, the
	 *             value cannot be cast to {@code long}, this method is called
	 *             on closed result set or there occurs some other error
	 */
	public long getLong(int columnIndex) throws OntoDriverException;

	/**
	 * Retrieves value from column with the specified label and returns it as
	 * {@code long}.
	 * 
	 * @param columnLabel
	 *            Label of the column
	 * @return {@code long} value
	 * @throws OntoDriverException
	 *             If there is no column with the specified label, the value
	 *             cannot be cast to {@code long}, this method is called on
	 *             closed result set or there occurs some other error
	 */
	public long getLong(String columnLabel) throws OntoDriverException;

	/**
	 * Retrieves value from column at the specified index and returns it as
	 * {@code Object}.
	 * 
	 * @param columnIndex
	 *            Column index, the first column has index 0
	 * @return column value cast to {@code Object}
	 * @throws OntoDriverException
	 *             If the {@code columnIndex} is not a valid column index, this
	 *             method is called on closed result set or there occurs some
	 *             other error
	 */
	public Object getObject(int columnIndex) throws OntoDriverException;

	/**
	 * Retrieves value from column with the specified label and returns it as
	 * {@code Object}.
	 * 
	 * @param columnLabel
	 *            Label of the column
	 * @return column value cast to {@code Object}
	 * @throws OntoDriverException
	 *             If there is no column with the specified label, this method
	 *             is called on closed result set or there occurs some other
	 *             error
	 */
	public Object getObject(String columnLabel) throws OntoDriverException;

	/**
	 * Retrieves value from column at the specified index and returns it as an
	 * instance of the specified class. </p>
	 * 
	 * The mechanism of transforming the value to the specified class is not
	 * specified, it can be merely type casting or calling a constructor of the
	 * specified type.
	 * 
	 * @param columnIndex
	 *            Column index, the first column has index 0
	 * @param cls
	 *            Requested class type
	 * @return Value of the column
	 * @throws OntoDriverException
	 *             If the {@code columnIndex} is not a valid column index, the
	 *             value cannot be cast to the specified type, this method is
	 *             called on closed result set or there occurs some other error
	 */
	public <T> T getObject(int columnIndex, Class<T> cls) throws OntoDriverException;

	/**
	 * Retrieves value from column with the specified label and returns it as an
	 * instance of the specified class. </p>
	 * 
	 * The mechanism of transforming the value to the specified class is not
	 * specified, it can be merely type casting or calling a constructor of the
	 * specified type.
	 * 
	 * @param columnLabel
	 *            Label of the column
	 * @param cls
	 *            Requested class type
	 * @return Value of the column
	 * @throws OntoDriverException
	 *             If there is no column with the specified label, the value
	 *             cannot be cast to the specified type, this method is called
	 *             on closed result set or there occurs some other error
	 */
	public <T> T getObject(String columnLabel, Class<T> cls) throws OntoDriverException;

	/**
	 * Retrieves index of the current row. </p>
	 * 
	 * The first row has index 0.
	 * 
	 * @return the current row index, -1 if there is no current row
	 * @throws OntoDriverException
	 *             If this method is called on closed result set or some other
	 *             error occurs
	 */
	public int getRowIndex() throws OntoDriverException;

	/**
	 * Retrieves value of column at the specified index and returns it as
	 * {@code short}.
	 * 
	 * @param columnIndex
	 *            Column index, the first column has index 0
	 * @return {@code short} value
	 * @throws OntoDriverException
	 *             If the {@code columnIndex} is not a valid column index, the
	 *             value cannot be cast to {@code short}, this method is called
	 *             on closed result set or there occurs some other error
	 */
	public short getShort(int columnIndex) throws OntoDriverException;

	/**
	 * Retrieves value of column with the specified label and returns it as
	 * {@code short}.
	 * 
	 * @param columnLabel
	 *            Label of the column
	 * @return {@code short} value
	 * @throws OntoDriverException
	 *             If there is no column with the specified label, the value
	 *             cannot be cast to {@code short}, this method is called on
	 *             closed result set or there occurs some other error
	 */
	public short getShort(String columnLabel) throws OntoDriverException;

	/**
	 * Retrieves the {@code Statement} that produced this {@code ResultSet}
	 * object. If this result set was generated some other way, this method will
	 * return {@code null}.
	 * 
	 * @return The {@code Statement} that produced this {@code ResultSet} or
	 *         null
	 * @throws OntoDriverException
	 *             If this method is called on closed result set or some other
	 *             error occurs
	 */
	public Statement getStatement() throws OntoDriverException;

	/**
	 * Retrieves value of column at the specified index and returns it as
	 * {@code String}.
	 * 
	 * @param columnIndex
	 *            Column index, the first column has index 0
	 * @return {@code String} value
	 * @throws OntoDriverException
	 *             If the {@code columnIndex} is not a valid column index, the
	 *             value cannot be cast to {@code String}, this method is called
	 *             on closed result set or there occurs some other error
	 */
	public String getString(int columnIndex) throws OntoDriverException;

	/**
	 * Retrieves value of column with the specified label and returns it as
	 * {@code String}.
	 * 
	 * @param columnLabel
	 *            Label of the column
	 * @return {@code String} value
	 * @throws OntoDriverException
	 *             If there is no column with the specified label, the value
	 *             cannot be cast to {@code String}, this method is called on
	 *             closed result set or there occurs some other error
	 */
	public String getString(String columnLabel) throws OntoDriverException;

	/**
	 * Returns true if the cursor is at the first row of this result set.
	 * 
	 * @return True if the cursor is at the first row, false otherwise
	 * @throws OntoDriverException
	 *             If this method is called on closed result set or some other
	 *             error occurs
	 */
	public boolean isFirst() throws OntoDriverException;

	/**
	 * Returns true if the cursor does not point at the last row in this result
	 * set.
	 * 
	 * @return True if there is at least one next row
	 * @throws OntoDriverException
	 *             If this method is called on closed result set or some other
	 *             error occurs
	 */
	public boolean hasNext() throws OntoDriverException;

	/**
	 * Move the cursor to the last row in this results set. </p>
	 * 
	 * Note that since the result set may be asynchronously updated, the last
	 * row does not have to always be the same.
	 * 
	 * @throws OntoDriverException
	 *             If this method is called on closed result set or some other
	 *             error occurs
	 */
	public void last() throws OntoDriverException;

	/**
	 * Move the cursor one row forward.
	 * 
	 * @throws OntoDriverException
	 *             If there are no more rows, this method is called on closed
	 *             result set or some other error occurs
	 */
	public void next() throws OntoDriverException;

	/**
	 * Move the cursor one row backwards.
	 * 
	 * @throws OntoDriverException
	 *             If the cursor is at the first row, this method is called on
	 *             closed result set or some other error occurs
	 */
	public void previous() throws OntoDriverException;

	/**
	 * Registers the specified {@code Observer} at this result set. </p>
	 * 
	 * The observer is notified whenever new results of ontology reasoning are
	 * available.
	 * 
	 * @param observer
	 *            The observer to register
	 * @throws OntoDriverException
	 *             If this method is called on closed result set or some other
	 *             error occurs
	 */
	public void registerObserver(Observer observer) throws OntoDriverException;

	/**
	 * Move the cursor a relative number of rows, either positive or negative.
	 * 
	 * @param rows
	 *            The number of rows to move the cursor of
	 * @throws OntoDriverException
	 *             If the {@code rows} number is not valid, this method is
	 *             called on closed result set or some other error occurs
	 */
	public void relative(int rows) throws OntoDriverException;

	/**
	 * Move the cursor to the specified row index. </p>
	 * 
	 * The first row has index 0.
	 * 
	 * @param rowIndex
	 *            Index to move the cursor to
	 * @throws OntoDriverException
	 *             If the index is not valid row index, this method is called on
	 *             closed result set or some other error occurs
	 */
	public void setRowIndex(int rowIndex) throws OntoDriverException;
}

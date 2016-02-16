/**
 * Copyright (C) 2011 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

import java.util.NoSuchElementException;
import java.util.Observer;

/**
 * Represents a set of results of a SPARQL query. </p>
 * <p>
 * This interface declares methods for getting values from a set of results of a SPARQL query issued to an ontology.
 *
 * @author kidney
 */
public interface ResultSet extends AutoCloseable {

    /**
     * Retrieves index of a column with the specified label.
     *
     * @param columnLabel Label of the column
     * @return index of the column or -1 if there is no such column
     * @throws IllegalStateException If called on a closed result set
     */
    public int findColumn(String columnLabel);

    /**
     * Gets the count of available columns. </p>
     * <p>
     * This number corresponds to the number of result variables bound in the query.
     *
     * @return Number of columns in the result set
     * @throws IllegalStateException If called on a closed result set
     */
    public int getColumnCount();

    /**
     * Move the cursor to the first row.
     *
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If some other error occurs
     */
    public void first() throws OntoDriverException;

    /**
     * Retrieves value from column at the specified index and returns it as a {@code boolean}.
     *
     * @param columnIndex Column index, the first column has index 0
     * @return {@code boolean} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If the {@code columnIndex} is not a valid column index, the value cannot be cast to
     *                               {@code boolean} or there occurs some other error
     */
    public boolean getBoolean(int columnIndex) throws OntoDriverException;

    /**
     * Retrieves value from column with the specified label and returns it as a {@code boolean}.
     *
     * @param columnLabel Label of the column
     * @return {@code boolean} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If there is no column with the specified label, the value cannot be cast to {@code
     *                               boolean} or there occurs some other error
     */
    public boolean getBoolean(String columnLabel) throws OntoDriverException;

    /**
     * Retrieves value from column at the specified index and returns it as {@code byte}.
     *
     * @param columnIndex Column index, the first column has index 0
     * @return {@code byte} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If the {@code columnIndex} is not a valid column index, the value cannot be cast to
     *                               {@code byte} or there occurs some other error
     */
    public byte getByte(int columnIndex) throws OntoDriverException;

    /**
     * Retrieves value from column with the specified label and returns it as {@code byte}.
     *
     * @param columnLabel Label of the column
     * @return {@code byte} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If there is no column with the specified label, the value cannot be cast to {@code
     *                               byte} or there occurs some other error
     */
    public byte getByte(String columnLabel) throws OntoDriverException;

    /**
     * Retrieves value from column at the specified index and returns it as {@code double}.
     *
     * @param columnIndex Column index, the first column has index 0
     * @return {@code double} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If the {@code columnIndex} is not a valid column index, the value cannot be cast to
     *                               {@code double} or there occurs some other error
     */
    public double getDouble(int columnIndex) throws OntoDriverException;

    /**
     * Retrieves value from column with the specified label and returns it as {@code double}.
     *
     * @param columnLabel Label of the column
     * @return {@code double} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If there is no column with the specified label, the value cannot be cast to {@code
     *                               double} or there occurs some other error
     */
    public double getDouble(String columnLabel) throws OntoDriverException;

    /**
     * Retrieves value from column at the specified index and returns it as {@code float}.
     *
     * @param columnIndex Column index, the first column has index 0
     * @return {@code float} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If the {@code columnIndex} is not a valid column index, the value cannot be cast to
     *                               {@code float} or there occurs some other error
     */
    public float getFloat(int columnIndex) throws OntoDriverException;

    /**
     * Retrieves value from column with the specified label and returns it as {@code float}.
     *
     * @param columnLabel Label of the column
     * @return {@code float} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If there is no column with the specified label, the value cannot be cast to {@code
     *                               float} or there occurs some other error
     */
    public float getFloat(String columnLabel) throws OntoDriverException;

    /**
     * Retrieves value from column at the specified index and returns it as {@code int}.
     *
     * @param columnIndex Column index, the first column has index 0
     * @return {@code int} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If the {@code columnIndex} is not a valid column index, the value cannot be cast to
     *                               {@code int} or there occurs some other error
     */
    public int getInt(int columnIndex) throws OntoDriverException;

    /**
     * Retrieves value from column with the specified label and returns it as {@code int}.
     *
     * @param columnLabel Label of the column
     * @return {@code int} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If there is no column with the specified label, the value cannot be cast to {@code
     *                               int} or there occurs some other error
     */
    public int getInt(String columnLabel) throws OntoDriverException;

    /**
     * Retrieves value from column at the specified index and returns it as {@code long}.
     *
     * @param columnIndex Column index, the first column has index 0
     * @return {@code long} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If the {@code columnIndex} is not a valid column index, the value cannot be cast to
     *                               {@code long} or there occurs some other error
     */
    public long getLong(int columnIndex) throws OntoDriverException;

    /**
     * Retrieves value from column with the specified label and returns it as {@code long}.
     *
     * @param columnLabel Label of the column
     * @return {@code long} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If there is no column with the specified label, the value cannot be cast to {@code
     *                               long} or there occurs some other error
     */
    public long getLong(String columnLabel) throws OntoDriverException;

    /**
     * Retrieves value from column at the specified index and returns it as {@code Object}.
     *
     * @param columnIndex Column index, the first column has index 0
     * @return column value cast to {@code Object}
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If the {@code columnIndex} is not a valid column index or there occurs some other
     *                               error
     */
    public Object getObject(int columnIndex) throws OntoDriverException;

    /**
     * Retrieves value from column with the specified label and returns it as {@code Object}.
     *
     * @param columnLabel Label of the column
     * @return column value cast to {@code Object}
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If there is no column with the specified label or there occurs some other error
     */
    public Object getObject(String columnLabel) throws OntoDriverException;

    /**
     * Retrieves value from column at the specified index and returns it as an instance of the specified class. </p>
     * <p>
     * The mechanism of transforming the value to the specified class is not specified, it can be merely type casting or
     * calling a constructor of the specified type.
     *
     * @param columnIndex Column index, the first column has index 0
     * @param cls         Requested class type
     * @return Value of the column
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If the {@code columnIndex} is not a valid column index, the value cannot be cast to
     *                               the specified type or there occurs some other error
     */
    public <T> T getObject(int columnIndex, Class<T> cls) throws OntoDriverException;

    /**
     * Retrieves value from column with the specified label and returns it as an instance of the specified class. </p>
     * <p>
     * The mechanism of transforming the value to the specified class is not specified, it can be merely type casting or
     * calling a constructor of the specified type.
     *
     * @param columnLabel Label of the column
     * @param cls         Requested class type
     * @return Value of the column.
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If there is no column with the specified label, the value cannot be cast to the
     *                               specified type or there occurs some other error
     */
    public <T> T getObject(String columnLabel, Class<T> cls) throws OntoDriverException;

    /**
     * Retrieves index of the current row. </p>
     * <p>
     * The first row has index 0.
     *
     * @return the current row index, -1 if there is no current row
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If some other error occurs
     */
    public int getRowIndex() throws OntoDriverException;

    /**
     * Retrieves value of column at the specified index and returns it as {@code short}.
     *
     * @param columnIndex Column index, the first column has index 0
     * @return {@code short} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If the {@code columnIndex} is not a valid column index, the value cannot be cast to
     *                               {@code short} or there occurs some other error
     */
    public short getShort(int columnIndex) throws OntoDriverException;

    /**
     * Retrieves value of column with the specified label and returns it as {@code short}.
     *
     * @param columnLabel Label of the column
     * @return {@code short} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If there is no column with the specified label, the value cannot be cast to {@code
     *                               short} or there occurs some other error
     */
    public short getShort(String columnLabel) throws OntoDriverException;

    /**
     * Retrieves the {@code Statement} that produced this {@code ResultSet} object. If this result set was generated
     * some other way, this method will return {@code null}.
     *
     * @return The {@code Statement} that produced this {@code ResultSet} or null
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If some other error occurs
     */
    public Statement getStatement() throws OntoDriverException;

    /**
     * Retrieves value of column at the specified index and returns it as {@code String}.
     *
     * @param columnIndex Column index, the first column has index 0
     * @return {@code String} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If the {@code columnIndex} is not a valid column index, the value cannot be cast to
     *                               {@code String} or there occurs some other error
     */
    public String getString(int columnIndex) throws OntoDriverException;

    /**
     * Retrieves value of column with the specified label and returns it as {@code String}.
     *
     * @param columnLabel Label of the column
     * @return {@code String} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If there is no column with the specified label, the value cannot be cast to {@code
     *                               String} or there occurs some other error
     */
    public String getString(String columnLabel) throws OntoDriverException;

    /**
     * Returns true if the cursor is at the first row of this result set.
     *
     * @return True if the cursor is at the first row, false otherwise
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If some other error occurs
     */
    public boolean isFirst() throws OntoDriverException;

    /**
     * Returns true if the cursor does not point at the last row in this result set.
     *
     * @return True if there is at least one next row
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If some other error occurs
     */
    public boolean hasNext() throws OntoDriverException;

    /**
     * Move the cursor to the last row in this results set. </p>
     * <p>
     * Note that since the result set may be asynchronously updated, the last row does not have to always be the same.
     *
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If some other error occurs
     */
    public void last() throws OntoDriverException;

    /**
     * Move the cursor one row forward.
     *
     * @throws NoSuchElementException If there are no more elements
     * @throws IllegalStateException  If called on a closed result set
     * @throws OntoDriverException    If some other error occurs
     */
    public void next() throws OntoDriverException;

    /**
     * Move the cursor one row backwards.
     *
     * @throws IllegalStateException If called on a closed result set or the cursor is at the first row
     * @throws OntoDriverException   If some other error occurs
     */
    public void previous() throws OntoDriverException;

    /**
     * Registers the specified {@code Observer} at this result set. </p>
     * <p>
     * The observer is notified whenever new results of ontology reasoning are available.
     *
     * @param observer The observer to register
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If or some other error occurs
     */
    public void registerObserver(Observer observer) throws OntoDriverException;

    /**
     * Move the cursor a relative number of rows, either positive or negative.
     *
     * @param rows The number of rows to move the cursor of
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If the {@code rows} number is not valid or some other error occurs
     */
    public void relative(int rows) throws OntoDriverException;

    /**
     * Move the cursor to the specified row index. </p>
     * <p>
     * The first row has index 0.
     *
     * @param rowIndex Index to move the cursor to
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If the index is not valid row index or some other error occurs
     */
    public void setRowIndex(int rowIndex) throws OntoDriverException;

    /**
     * Closes this result set releasing any sub-resources it holds. </p>
     * <p>
     * After closing the result set is not usable any more and calling methods on it (except {@code close} and {@code
     * isOpen}) will result in {@code OntoDriverException}. </p>
     * <p>
     * Calling {@code close} on already closed resource does nothing. </p>
     * <p>
     * Calling this method also results in immediate disconnection of all registered observers and cancellation of any
     * running reasoning associated with this result set.
     *
     * @throws OntoDriverException If an ontology access error occurs.
     */
    @Override
    public void close() throws OntoDriverException;

    /**
     * Retrieves status of this result set. </p>
     *
     * @return {@code true} if the resource is open, {@code false} otherwise
     */
    public boolean isOpen();
}

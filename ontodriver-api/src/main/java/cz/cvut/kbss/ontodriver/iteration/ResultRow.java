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

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

import java.util.List;

/**
 * Represents a single row in a {@link cz.cvut.kbss.ontodriver.ResultSet}.
 * <p>
 * This interface replicates the value retrieval API of {@code ResultSet} and serves as a view of the current row in a
 * result set.
 * <p>
 * The main purpose of this interface is to support iteration over result sets.
 */
public interface ResultRow {

    /**
     * Retrieves index of a column with the specified label.
     *
     * @param columnLabel Label of the column
     * @return index of the column or -1 if there is no such column
     * @throws IllegalStateException If called on a closed result set
     */
    int findColumn(String columnLabel);

    /**
     * Gets the count of available columns.
     * <p>
     * This number corresponds to the number of result variables bound in the query.
     *
     * @return Number of columns in the result set
     * @throws IllegalStateException If called on a closed result set
     */
    int getColumnCount();

    /**
     * Gets the names of the available columns.
     * <p>
     * The indices of the names in the returned list correspond to their indices, as they would be returned, for
     * example, by {@link #findColumn(String)}.
     *
     * @return List of column names
     */
    List<String> getColumnNames();

    /**
     * Checks whether a value at the specified index is bound in the current result row.
     * <p>
     * Note that this method will return {@code false} also in case the index is out of range of the variables known to
     * the result set as a whole.
     *
     * @param variableIndex Index of the variable
     * @return {@code true} when value is bound in the current row, {@code false} otherwise
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   When unable to resolve binding status
     */
    boolean isBound(int variableIndex) throws OntoDriverException;

    /**
     * Checks whether a value of the specified variable is bound in the current result row.
     * <p>
     * Note that this method will return {@code false} also in case the variable is not known to the result set at all.
     *
     * @param variableName Variable name
     * @return {@code true} when value is bound in the current row, {@code false} otherwise
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   When unable to resolve binding status
     */
    boolean isBound(String variableName) throws OntoDriverException;

    /**
     * Retrieves value from column at the specified index and returns it as a {@code boolean}.
     *
     * @param columnIndex Column index, the first column has index 0
     * @return {@code boolean} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If the {@code columnIndex} is not a valid column index, the value cannot be cast to
     *                               {@code boolean} or there occurs some other error
     */
    boolean getBoolean(int columnIndex) throws OntoDriverException;

    /**
     * Retrieves value from column with the specified label and returns it as a {@code boolean}.
     *
     * @param columnLabel Label of the column
     * @return {@code boolean} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If there is no column with the specified label, the value cannot be cast to
     *                               {@code boolean} or there occurs some other error
     */
    boolean getBoolean(String columnLabel) throws OntoDriverException;

    /**
     * Retrieves value from column at the specified index and returns it as {@code byte}.
     *
     * @param columnIndex Column index, the first column has index 0
     * @return {@code byte} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If the {@code columnIndex} is not a valid column index, the value cannot be cast to
     *                               {@code byte} or there occurs some other error
     */
    byte getByte(int columnIndex) throws OntoDriverException;

    /**
     * Retrieves value from column with the specified label and returns it as {@code byte}.
     *
     * @param columnLabel Label of the column
     * @return {@code byte} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If there is no column with the specified label, the value cannot be cast to
     *                               {@code byte} or there occurs some other error
     */
    byte getByte(String columnLabel) throws OntoDriverException;

    /**
     * Retrieves value from column at the specified index and returns it as {@code double}.
     *
     * @param columnIndex Column index, the first column has index 0
     * @return {@code double} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If the {@code columnIndex} is not a valid column index, the value cannot be cast to
     *                               {@code double} or there occurs some other error
     */
    double getDouble(int columnIndex) throws OntoDriverException;

    /**
     * Retrieves value from column with the specified label and returns it as {@code double}.
     *
     * @param columnLabel Label of the column
     * @return {@code double} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If there is no column with the specified label, the value cannot be cast to
     *                               {@code double} or there occurs some other error
     */
    double getDouble(String columnLabel) throws OntoDriverException;

    /**
     * Retrieves value from column at the specified index and returns it as {@code float}.
     *
     * @param columnIndex Column index, the first column has index 0
     * @return {@code float} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If the {@code columnIndex} is not a valid column index, the value cannot be cast to
     *                               {@code float} or there occurs some other error
     */
    float getFloat(int columnIndex) throws OntoDriverException;

    /**
     * Retrieves value from column with the specified label and returns it as {@code float}.
     *
     * @param columnLabel Label of the column
     * @return {@code float} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If there is no column with the specified label, the value cannot be cast to
     *                               {@code float} or there occurs some other error
     */
    float getFloat(String columnLabel) throws OntoDriverException;

    /**
     * Retrieves value from column at the specified index and returns it as {@code int}.
     *
     * @param columnIndex Column index, the first column has index 0
     * @return {@code int} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If the {@code columnIndex} is not a valid column index, the value cannot be cast to
     *                               {@code int} or there occurs some other error
     */
    int getInt(int columnIndex) throws OntoDriverException;

    /**
     * Retrieves value from column with the specified label and returns it as {@code int}.
     *
     * @param columnLabel Label of the column
     * @return {@code int} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If there is no column with the specified label, the value cannot be cast to
     *                               {@code int} or there occurs some other error
     */
    int getInt(String columnLabel) throws OntoDriverException;

    /**
     * Retrieves value from column at the specified index and returns it as {@code long}.
     *
     * @param columnIndex Column index, the first column has index 0
     * @return {@code long} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If the {@code columnIndex} is not a valid column index, the value cannot be cast to
     *                               {@code long} or there occurs some other error
     */
    long getLong(int columnIndex) throws OntoDriverException;

    /**
     * Retrieves value from column with the specified label and returns it as {@code long}.
     *
     * @param columnLabel Label of the column
     * @return {@code long} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If there is no column with the specified label, the value cannot be cast to
     *                               {@code long} or there occurs some other error
     */
    long getLong(String columnLabel) throws OntoDriverException;

    /**
     * Retrieves value from column at the specified index and returns it as {@code Object}.
     *
     * @param columnIndex Column index, the first column has index 0
     * @return column value cast to {@code Object}
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If the {@code columnIndex} is not a valid column index or there occurs some other
     *                               error
     */
    Object getObject(int columnIndex) throws OntoDriverException;

    /**
     * Retrieves value from column with the specified label and returns it as {@code Object}.
     *
     * @param columnLabel Label of the column
     * @return column value cast to {@code Object}
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If there is no column with the specified label or there occurs some other error
     */
    Object getObject(String columnLabel) throws OntoDriverException;

    /**
     * Retrieves value from column at the specified index and returns it as an instance of the specified class.
     * <p>
     * The mechanism of transforming the value to the specified class is not specified, it can be merely type casting or
     * calling a constructor of the specified type.
     *
     * @param columnIndex Column index, the first column has index 0
     * @param cls         Requested class type
     * @param <T>         Return type
     * @return Value of the column
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If the {@code columnIndex} is not a valid column index, the value cannot be cast to
     *                               the specified type or there occurs some other error
     */
    <T> T getObject(int columnIndex, Class<T> cls) throws OntoDriverException;

    /**
     * Retrieves value from column with the specified label and returns it as an instance of the specified class.
     * <p>
     * The mechanism of transforming the value to the specified class is not specified, it can be merely type casting or
     * calling a constructor of the specified type.
     *
     * @param columnLabel Label of the column
     * @param cls         Requested class type
     * @param <T>         Return type
     * @return Value of the column.
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If there is no column with the specified label, the value cannot be cast to the
     *                               specified type or there occurs some other error
     */
    <T> T getObject(String columnLabel, Class<T> cls) throws OntoDriverException;

    /**
     * Retrieves value of column at the specified index and returns it as {@code short}.
     *
     * @param columnIndex Column index, the first column has index 0
     * @return {@code short} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If the {@code columnIndex} is not a valid column index, the value cannot be cast to
     *                               {@code short} or there occurs some other error
     */
    short getShort(int columnIndex) throws OntoDriverException;

    /**
     * Retrieves value of column with the specified label and returns it as {@code short}.
     *
     * @param columnLabel Label of the column
     * @return {@code short} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If there is no column with the specified label, the value cannot be cast to
     *                               {@code short} or there occurs some other error
     */
    short getShort(String columnLabel) throws OntoDriverException;

    /**
     * Retrieves value of column at the specified index and returns it as {@code String}.
     *
     * @param columnIndex Column index, the first column has index 0
     * @return {@code String} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If the {@code columnIndex} is not a valid column index, the value cannot be cast to
     *                               {@code String} or there occurs some other error
     */
    String getString(int columnIndex) throws OntoDriverException;

    /**
     * Retrieves value of column with the specified label and returns it as {@code String}.
     *
     * @param columnLabel Label of the column
     * @return {@code String} value
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If there is no column with the specified label, the value cannot be cast to
     *                               {@code String} or there occurs some other error
     */
    String getString(String columnLabel) throws OntoDriverException;


    /**
     * Gets the index of this row (first row has index 0).
     *
     * @return Index of this row
     * @throws IllegalStateException If called on a closed result set
     * @throws OntoDriverException   If some other error occurs
     */
    int getIndex() throws OntoDriverException;
}

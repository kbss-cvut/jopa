/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

/**
 * This interface extends the {@code Statement} and adds the possibility to parametrize queries
 * <p>
 * Implementations are also expected to support at least a basic level of character escaping (e.g. quotes) and other
 * injection-protection methods.
 */
public interface PreparedStatement extends Statement {

    /**
     * Executes query represented by this statement.
     *
     * @return ResultSet of the query
     * @throws OntoDriverException   If an ontology access error occurs
     * @throws IllegalStateException If called on a closed statement
     */
    ResultSet executeQuery() throws OntoDriverException;

    /**
     * Executes an update represented by this statement.
     *
     * @throws OntoDriverException   If an ontology access error occurs
     * @throws IllegalStateException If called on a closed statement
     */
    void executeUpdate() throws OntoDriverException;

    /**
     * Sets value of binding with the specified name.
     *
     * @param binding Binding name
     * @param value   The value of the parameter
     * @throws OntoDriverException   If there is no such binding in the statement or some other error occurs
     * @throws IllegalStateException If called on a closed statement
     */
    void setObject(String binding, Object value) throws OntoDriverException;

    /**
     * Clears the currently set parameters.
     *
     * @throws OntoDriverException   If an ontology access error occurs
     * @throws IllegalStateException If called on a closed statement
     */
    void clearParameters() throws OntoDriverException;
}

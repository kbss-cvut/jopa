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
package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;

/**
 * Procedure functional interface.
 * <p>
 * Executes a method which takes no arguments and returns no result.
 */
@FunctionalInterface
interface Procedure {
    /**
     * Executes the procedure.
     *
     * @throws Rdf4jDriverException Indicates an exception thrown by the underlying API
     */
    void execute() throws Rdf4jDriverException;
}

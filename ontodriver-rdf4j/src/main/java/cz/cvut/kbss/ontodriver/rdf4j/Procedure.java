/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;

/**
 * Procedure functional interface.
 * <p>
 * Executes a method which takes no arguments and returns no result.
 */
@FunctionalInterface
public interface Procedure {
    /**
     * Executes the procedure.
     *
     * @throws Rdf4jDriverException Indicates an exception thrown by the underlying API
     */
    void execute() throws Rdf4jDriverException;
}

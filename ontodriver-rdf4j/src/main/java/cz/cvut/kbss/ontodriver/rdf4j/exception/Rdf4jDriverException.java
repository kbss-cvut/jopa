/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.rdf4j.exception;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

/**
 * This is a generic, checked exception thrown by the RDF4J driver.
 * <p>
 * It extends the {@link OntoDriverException}, so we can throw it instead of that one.
 */
public class Rdf4jDriverException extends OntoDriverException {

    public Rdf4jDriverException() {
    }

    public Rdf4jDriverException(String message) {
        super(message);
    }

    public Rdf4jDriverException(Throwable cause) {
        super(cause);
    }

    public Rdf4jDriverException(String message, Throwable cause) {
        super(message, cause);
    }
}

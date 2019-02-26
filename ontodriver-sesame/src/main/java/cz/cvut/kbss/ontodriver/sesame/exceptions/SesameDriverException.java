/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.sesame.exceptions;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

/**
 * This is a generic checked exception thrown by the Sesame driver.
 * <p>
 * It extends the {@link OntoDriverException}, so we can throw it instead of that one.
 */
public class SesameDriverException extends OntoDriverException {

    private static final long serialVersionUID = 4771575441559318165L;

    public SesameDriverException() {
    }

    public SesameDriverException(String message) {
        super(message);
    }

    public SesameDriverException(Throwable cause) {
        super(cause);
    }

    public SesameDriverException(String message, Throwable cause) {
        super(message, cause);
    }
}

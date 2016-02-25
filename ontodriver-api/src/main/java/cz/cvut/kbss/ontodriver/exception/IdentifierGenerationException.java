/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.exception;

public class IdentifierGenerationException extends RuntimeException {

	private static final long serialVersionUID = -8466375018813594628L;

	public IdentifierGenerationException() {
	}

	public IdentifierGenerationException(String message) {
		super(message);
	}

	public IdentifierGenerationException(Throwable cause) {
		super(cause);
	}

	public IdentifierGenerationException(String message, Throwable cause) {
		super(message, cause);
	}
}

/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.exceptions;

/**
 * Indicates that a modification to an attribute could not be performed.
 *
 * This exception is raised when an attribute is read only for some reason and the application attempts to change its value.
 *
 * Concrete subclasses will provide more context as to why this exception was thrown.
 */
public class AttributeModificationForbiddenException extends OWLPersistenceException {

    public AttributeModificationForbiddenException(String message) {
        super(message);
    }
}

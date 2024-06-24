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

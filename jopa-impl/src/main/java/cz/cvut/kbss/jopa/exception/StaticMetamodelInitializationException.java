/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.exception;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;

/**
 * Indicates that the static metamodel could not be initialized.
 */
public class StaticMetamodelInitializationException extends OWLPersistenceException {

    public StaticMetamodelInitializationException(String message) {
        super(message);
    }

    public StaticMetamodelInitializationException(String message, Throwable cause) {
        super(message, cause);
    }
}

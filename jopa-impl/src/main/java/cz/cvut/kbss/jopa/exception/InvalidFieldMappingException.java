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
package cz.cvut.kbss.jopa.exception;

import java.lang.reflect.Field;

/**
 * Signals that an entity field mapping is not valid.
 * <p>
 * It can mean for example that its type does not correspond to the mapping (singular field for plural mapping,
 * non-Set for a {@link cz.cvut.kbss.jopa.model.annotations.Types} field) etc.
 */
public class InvalidFieldMappingException extends MetamodelInitializationException {

    public InvalidFieldMappingException(String message) {
        super(message);
    }

    public InvalidFieldMappingException(Field field, String reason) {
        super("Invalid mapping of field " + field + ", reason: " + reason);
    }
}

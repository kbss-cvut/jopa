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

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;

/**
 * Indicates that an attempt has been made to instantiate a class that is abstract in terms of JOPA.
 * <p>
 * This could be, for example, a {@link cz.cvut.kbss.jopa.model.annotations.MappedSuperclass} type, an {@link
 * cz.cvut.kbss.jopa.model.metamodel.EntityType} representing an interface or an abstract entity class that needs to be
 * inherited for instantiation.
 */
public class AbstractTypeException extends OWLPersistenceException {

    public AbstractTypeException(String message) {
        super(message);
    }
}

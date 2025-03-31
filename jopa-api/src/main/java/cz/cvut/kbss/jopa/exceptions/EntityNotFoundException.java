/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.model.EntityManager;

/**
 * Thrown by the persistence provider when an entity reference obtained by
 * {@link EntityManager#getReference(Class, Object)} is accessed but the entity does not exist. Thrown when
 * {@link EntityManager#refresh} is called and the object no longer exists in the database.
 * <p>
 * The current transaction, if one is active and the persistence context has been joined to it, will be marked for
 * rollback.
 */
public class EntityNotFoundException extends OWLPersistenceException {

    public EntityNotFoundException(String message) {
        super(message);
    }
}

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

import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;

/**
 * Thrown by the persistence provider when {@link Query#getSingleResult()} or {@link TypedQuery#getSingleResult()} is
 * executed on a query and there is no result to return.
 * <p>
 * This exception will not cause the current transaction, if one is active, to be marked for rollback.
 */
public class NoResultException extends OWLPersistenceException {

    public NoResultException(String message) {
        super(message);
    }
}

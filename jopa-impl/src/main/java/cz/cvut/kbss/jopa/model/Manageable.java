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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.sessions.UnitOfWork;

/**
 * Interface dynamically assigned to entity classes so that their instances may be attached to a persistence context.
 */
public interface Manageable {

    /**
     * Sets persistence context to this instance.
     * <p>
     * Done when an object enters the persistence context
     *
     * @param uow Persistence context
     */
    void setPersistenceContext(UnitOfWork uow);

    /**
     * Gets the persistence context associated with this entity instance.
     *
     * @return Persistence context instance
     */
    UnitOfWork getPersistenceContext();
}

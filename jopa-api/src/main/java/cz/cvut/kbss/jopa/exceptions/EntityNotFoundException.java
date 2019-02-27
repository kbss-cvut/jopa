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
package cz.cvut.kbss.jopa.exceptions;

import cz.cvut.kbss.jopa.model.EntityManager;

/**
 * Thrown when {@link EntityManager#refresh(Object)}  is called and the object no longer exists in the database.
 * <p>
 * The current transaction, if one is active and the persistence context has been joined to it, will be marked for rollback.
 */
public class EntityNotFoundException extends OWLPersistenceException {

    public EntityNotFoundException(String message) {
        super(message);
    }
}

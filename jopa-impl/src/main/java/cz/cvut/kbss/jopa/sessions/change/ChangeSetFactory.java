/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.sessions.change;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkChangeSet;

public class ChangeSetFactory {

    private ChangeSetFactory() {
        throw new AssertionError();
    }

    /**
     * Creates change set for the specified UnitOfWork.
     *
     * @return New change set
     */
    public static UnitOfWorkChangeSet createUoWChangeSet() {
        return new UnitOfWorkChangeSetImpl();
    }

    /**
     * Creates new change set for the specified original-clone pair.
     *
     * @param original
     *            Original object
     * @param clone
     *            Clone
     * @param descriptor
     *            Entity descriptor
     * @return New object change set
     */
    public static ObjectChangeSet createObjectChangeSet(Object original, Object clone, Descriptor descriptor) {
        return new ObjectChangeSetImpl(original, clone, descriptor);
    }

    /**
     * Creates a new change set representing object removal.
     * @param toDelete Object to remove
     * @param descriptor Entity descriptor
     * @return New object change set
     */
    public static ObjectChangeSet createDeleteObjectChangeSet(Object toDelete, Descriptor descriptor) {
        return new DeleteObjectChangeSet(toDelete, descriptor);
    }
}

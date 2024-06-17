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
package cz.cvut.kbss.jopa.sessions.change;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;

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
        return new UnitOfWorkChangeSet();
    }

    /**
     * Creates new change set for the specified original-clone pair.
     *
     * @param original   Original object
     * @param clone      Clone
     * @param descriptor Entity descriptor
     * @return New object change set
     */
    public static ObjectChangeSet createObjectChangeSet(Object original, Object clone, Descriptor descriptor) {
        return new ObjectChangeSet(original, clone, descriptor);
    }

    /**
     * Creates a change representing object deletion.
     *
     * @param clone   Deleted object clone
     * @param original Original of the deleted object
     * @param descriptor Entity descriptor
     * @return Delete object change
     */
    public static DeleteObjectChange createDeleteObjectChange(Object clone, Object original, Descriptor descriptor) {
        return new DeleteObjectChange(clone, original, descriptor);
    }

    /**
     * Creates a change representing object persist.
     *
     * @param newObject  Persisted object
     * @param descriptor Entity descriptor
     * @return New object change
     */
    public static NewObjectChange createNewObjectChange(Object newObject, Descriptor descriptor) {
        return new NewObjectChange(newObject, descriptor);
    }
}

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
package cz.cvut.kbss.jopa.api;

public interface ChangeManager {

    /**
     * This method does a quick check to find out whether there are any changes
     * to the clone. It does a object value comparison, i. e. it compares each
     * value of the clone against the original value and returns true if a
     * change is found.
     *
     * @param original The original object.
     * @param clone    The clone, whose changes we are looking for.
     * @return True if there is a change (at least one) or false, if the values are identical.
     */
    boolean hasChanges(Object original, Object clone);

    /**
     * Calculates the changes that happened to the clone object. If there are no
     * changes, null is returned. The changes are written into the change set
     * passed in as argument.
     *
     * @param changeSet Contains references to the original and clone objects. Into this change set the changes should
     *                  be propagated
     * @return {@code true} if there were any changes, {@code false} otherwise
     * @throws NullPointerException     If {@code changeSet} is {@code null}
     */
    boolean calculateChanges(ObjectChangeSet changeSet);

}

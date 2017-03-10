/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.sessions;

/**
 * This interface defines methods for merging changes from clones to originals.
 */
public interface MergeManager {

    /**
     * Merge changes from one ObjectChangeSet, which represents the changes made
     * to clone, into the original object.
     *
     * @param changeSet ObjectChangeSet
     * @return Object The merged object.
     */
    Object mergeChangesOnObject(ObjectChangeSet changeSet);

    /**
     * Merges changes specified by the {@link ObjectChangeSet} into the original object.
     * <p>
     * The clone in this case is a detached instance being merged into the persistence context.
     *
     * @param changeSet The change set to merge
     * @return The instance into which changes were merged
     */
    Object mergeChangesFromDetached(ObjectChangeSet changeSet);

    /**
     * Merge changes from the provided UnitOfWorkChangeSet into the target
     * session.
     *
     * @param changeSet UnitOfWorkChangeSet
     */
    void mergeChangesFromChangeSet(UnitOfWorkChangeSet changeSet);

    /**
     * Merge newly created object into the shared session cache.
     *
     * @param changeSet ObjectChangeSet
     */
    void mergeNewObject(ObjectChangeSet changeSet);

}

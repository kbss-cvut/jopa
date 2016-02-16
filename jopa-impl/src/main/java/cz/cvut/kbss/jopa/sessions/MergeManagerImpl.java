/**
 * Copyright (C) 2011 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.net.URI;

public class MergeManagerImpl implements MergeManager {

	// The UnitOfWork instance for this MergeManager
	protected UnitOfWorkImpl uow;

	protected CloneBuilder builder;

	public MergeManagerImpl(UnitOfWorkImpl session) {
		this.uow = session;
		this.builder = new CloneBuilderImpl(session);
	}

	protected void deleteObjectFromCache(ObjectChangeSet changeSet) {
		Object original = changeSet.getChangedObject();
		if (original != null) {
			uow.removeObjectFromCache(original, changeSet.getEntityContext());
		}
	}

	public Object mergeChangesOnObject(Object clone, ObjectChangeSet changeSet) {
		if (changeSet == null) {
			throw new OWLPersistenceException("Change Set in Merge Manager null.");
		}
		if (clone == null) {
			return null;
		}

		Object original = changeSet.getChangedObject();
		if (original == null) {
			// If the original is null, then we may have a new object
			// but this should not happen since new objects are handled
			// separately
			if (uow.isObjectNew(clone)) {
				mergeNewObject(changeSet);
			} else {
				throw new OWLPersistenceException("Cannot find the original object.");
			}
		} else {
			this.builder.mergeChanges(original, changeSet);
		}
		return clone;
	}

	public void mergeChangesFromChangeSet(UnitOfWorkChangeSet changeSet) {
		for (ObjectChangeSet objectChangeSet : changeSet.getExistingObjectsChanges()) {
				Object clone = objectChangeSet.getCloneObject();
				mergeChangesOnObject(clone, objectChangeSet);
		}
		changeSet.getNewObjects().forEach(this::mergeNewObject);
		changeSet.getDeletedObjects().forEach(this::deleteObjectFromCache);

	}

	public void mergeNewObject(ObjectChangeSet changeSet) {
		if (changeSet == null) {
			return;
		}
		Object clone = changeSet.getCloneObject();
		if (!changeSet.isNew()) {
			mergeChangesOnObject(clone, changeSet);
		}
		// Put the original object into the shared session cache
		Object newObject = changeSet.getChangedObject();
		final Object primaryKey = EntityPropertiesUtils.getPrimaryKey(newObject, uow.getMetamodel());
		final URI context = changeSet.getEntityContext();
		uow.putObjectIntoCache(primaryKey, newObject, context);
	}

}

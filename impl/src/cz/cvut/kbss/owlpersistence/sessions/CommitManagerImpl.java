package cz.cvut.kbss.owlpersistence.sessions;

import java.util.Iterator;
import java.util.Map;
import cz.cvut.kbss.owlpersistence.accessors.OntologyAccessor;

public class CommitManagerImpl implements CommitManager {

	// Accessor to the underlying ontology
	protected OntologyAccessor accessor;

	public CommitManagerImpl(OntologyAccessor accessor) {
		this.accessor = accessor;
	}

	public void commitNewEntities(UnitOfWorkChangeSet changeSet) {
		if (changeSet == null) {
			return;
		}
		Map<?, ?> newEnts = changeSet.getNewObjectChangeSets();
		Iterator<?> it = newEnts.keySet().iterator();
		UnitOfWork uow = (UnitOfWork) ((UnitOfWorkChangeSetImpl) changeSet)
				.getSession();
		while (it.hasNext()) {
			Map<ObjectChangeSet, ObjectChangeSet> objChangeSets = (Map<ObjectChangeSet, ObjectChangeSet>) newEnts
					.get(it.next());
			Iterator<?> it2 = objChangeSets.keySet().iterator();
			while (it2.hasNext()) {
				ObjectChangeSet ocs = (ObjectChangeSet) it2.next();
				Object entity = ocs.getChangedObject();
				if (entity == null) {
					continue;
				}
				this.accessor.persistEntity(entity, uow);
			}
		}

	}

	public void commitChanges(UnitOfWorkChangeSet changeSet) {
		if (changeSet == null) {
			return;
		}
		if (changeSet.hasNew()) {
			this.commitNewEntities(changeSet);
		}
		if (changeSet.hasDeletedObjects()) {
			this.commitDeleted(changeSet);
		}
		if (changeSet.hasChanges()) {
			this.commitManagedChanges(changeSet);
		}
		this.accessor.saveWorkingOntology();
	}

	/**
	 * Commits the changes to the managed objects.
	 * 
	 * @param changeSet
	 *            The change set from which changes shall be propagated to the
	 *            ontology.
	 */
	protected void commitManagedChanges(UnitOfWorkChangeSet changeSet) {
		Map<?, ?> setsForClasses = changeSet.getObjectChanges();
		Iterator<?> it = setsForClasses.keySet().iterator();
		UnitOfWork uow = (UnitOfWork) ((UnitOfWorkChangeSetImpl) changeSet)
				.getSession();
		while (it.hasNext()) {
			Class<?> cls = (Class<?>) it.next();
			Map<ObjectChangeSet, ObjectChangeSet> chSet = (Map<ObjectChangeSet, ObjectChangeSet>) setsForClasses
					.get(cls);
			Iterator<ObjectChangeSet> it2 = chSet.keySet().iterator();
			while (it2.hasNext()) {
				ObjectChangeSet oSet = it2.next();
				// We can simply save the clone values, there is no difference
				Object entity = oSet.getCloneObject();
				if (entity != null) {
					this.accessor.persistExistingEntity(entity, uow);
				}
			}
		}
	}

	public void commitDeleted(UnitOfWorkChangeSet changeSet) {
		if (changeSet == null) {
			return;
		}
		Map<?, ?> changeSets = changeSet.getDeletedObjects();
		Iterator<?> it = changeSets.keySet().iterator();
		while (it.hasNext()) {
			ObjectChangeSet st = (ObjectChangeSet) it.next();
			Object toDelete = st.getChangedObject();
			if (toDelete != null) {
				this.accessor.removeEntity(toDelete);
			}
		}

	}

}

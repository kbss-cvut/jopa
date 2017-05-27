package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.merge.DetachedValueMerger;
import cz.cvut.kbss.jopa.sessions.merge.ValueMerger;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

class DetachedInstanceMerger {

    private final ValueMerger valueMerger;

    DetachedInstanceMerger(UnitOfWorkImpl uow) {
        this.valueMerger = new DetachedValueMerger(uow);
    }

    /**
     * Merges changes from the detached instance being merged into persistence context (clone in the specified change
     * set) into the corresponding managed instance (original in the change set).
     *
     * @param changeSet Set of changes to apply to the managed instance
     * @return Managed instance with changes merged into it
     */
    Object mergeChangesFromDetachedToManagedInstance(ObjectChangeSet changeSet, Descriptor descriptor) {
        assert changeSet != null;
        assert changeSet.getCloneObject() != null;
        final Object original = changeSet.getChangedObject();
        assert original != null;

        for (ChangeRecord change : changeSet.getChanges()) {
            final FieldSpecification<?, ?> fs = change.getAttribute();
            final Object origValue = EntityPropertiesUtils.getAttributeValue(fs, original);
            valueMerger
                    .mergeValue(fs, original, origValue, change.getNewValue(), descriptor.getAttributeDescriptor(fs));
        }
        return original;
    }
}

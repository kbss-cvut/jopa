package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.adapters.IndirectCollection;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.merge.DefaultValueMerger;
import cz.cvut.kbss.jopa.sessions.merge.ValueMerger;
import cz.cvut.kbss.jopa.utils.CollectionFactory;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

/**
 * Merges changes during instance refresh.
 * <p>
 * This means overwriting any changes made to the entity.
 */
class RefreshInstanceMerger {

    private final ValueMerger merger;

    private final CollectionFactory collectionFactory;

    RefreshInstanceMerger(CollectionFactory collectionFactory) {
        this.collectionFactory = collectionFactory;
        this.merger = new DefaultValueMerger();
    }

    /**
     * Merges changes in the opposite direction, i.e. changes made on the clone are overwritten by the original values.
     *
     * @param changeSet Changes done
     */
    void mergeChanges(ObjectChangeSet changeSet) {
        final Object source = changeSet.getChangedObject();
        final Object target = changeSet.getCloneObject();
        for (ChangeRecord change : changeSet.getChanges()) {
            final FieldSpecification<?, ?> att = change.getAttribute();
            final Object sourceValue = EntityPropertiesUtils.getAttributeValue(att, source);
            if (sourceValue instanceof IndirectCollection) {
                final IndirectCollection col = (IndirectCollection) sourceValue;
                final IndirectCollection<?> ic = collectionFactory
                        .createIndirectCollection(col.getReferencedCollection(), target, att.getJavaField());
                merger.mergeValue(att, target, null, ic, null);
            } else {
                merger.mergeValue(att, target, null, sourceValue, null);
            }
        }
    }
}

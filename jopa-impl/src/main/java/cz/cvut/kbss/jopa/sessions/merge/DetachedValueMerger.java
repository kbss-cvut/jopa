package cz.cvut.kbss.jopa.sessions.merge;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;

import java.util.Collection;
import java.util.Map;

public class DetachedValueMerger implements ValueMerger {

    private final UnitOfWorkImpl uow;

    private final ValueMerger defaultValueMerger;
    private final ManagedTypeValueMerger managedTypeMerger;
    private final CollectionValueMerger collectionMerger;
    private final MapValueMerger mapValueMerger;

    public DetachedValueMerger(UnitOfWorkImpl uow) {
        assert uow != null;
        this.uow = uow;
        this.defaultValueMerger = new DefaultValueMerger();
        this.managedTypeMerger = new ManagedTypeValueMerger(uow);
        this.collectionMerger = new CollectionValueMerger(uow, managedTypeMerger);
        this.mapValueMerger = new MapValueMerger();
    }

    @Override
    public void mergeValue(FieldSpecification<?, ?> att, Object target, Object originalValue, Object mergedValue,
                           Descriptor attributeDescriptor) {
        getMerger(att).mergeValue(att, target, originalValue, mergedValue, attributeDescriptor);
    }

    private ValueMerger getMerger(FieldSpecification<?, ?> att) {
        if (uow.isTypeManaged(att.getJavaType())) {
            return managedTypeMerger;
        } else if (Collection.class.isAssignableFrom(att.getJavaType())) {
            return collectionMerger;
        } else if (Map.class.isAssignableFrom(att.getJavaType())) {
            return mapValueMerger;
        } else {
            return defaultValueMerger;
        }
    }
}

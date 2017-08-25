package cz.cvut.kbss.jopa.sessions.merge;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

class ManagedTypeValueMerger implements ValueMerger {

    private final UnitOfWorkImpl uow;

    ManagedTypeValueMerger(UnitOfWorkImpl uow) {
        this.uow = uow;
    }

    @Override
    public void mergeValue(FieldSpecification<?, ?> att, Object target, Object originalValue, Object mergedValue,
                           Descriptor attributeDescriptor) {
        final Object toSet = getValueToSet(mergedValue, attributeDescriptor);
        EntityPropertiesUtils.setFieldValue(att.getJavaField(), target, toSet);
    }

    Object getValueToSet(Object mergedValue, Descriptor descriptor) {
        if (mergedValue == null) {
            return null;
        }
        final Object identifier = EntityPropertiesUtils.getIdentifier(mergedValue, uow.getMetamodel());
        if (identifier == null) {
            return mergedValue;
        }
        final Class<?> type = mergedValue.getClass();
        final Object managedInstance = uow.readObject(type, identifier, descriptor);
        // If the object cannot be found, it is a new one (not yet registered), which we can assign directly
        return managedInstance != null ? managedInstance : mergedValue;
    }
}

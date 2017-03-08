package cz.cvut.kbss.jopa.sessions.merge;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.jopa.utils.CollectionFactory;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.MetamodelUtils;

import java.util.Collection;

class CollectionValueMerger implements ValueMerger {

    private final UnitOfWorkImpl uow;
    private final ManagedTypeValueMerger managedTypeMerger;

    CollectionValueMerger(UnitOfWorkImpl uow, ManagedTypeValueMerger managedTypeMerger) {
        this.uow = uow;
        this.managedTypeMerger = managedTypeMerger;
    }

    @Override
    public void mergeValue(FieldSpecification<?, ?> att, Object target, Object originalValue, Object mergedValue,
                           Descriptor attributeDescriptor) {
        final Collection<?> mergedCol = (Collection<?>) mergedValue;
        if (mergedCol == null) {
            EntityPropertiesUtils.setFieldValue(att.getJavaField(), target, null);
            return;
        }

        final Collection<Object> newValue = CollectionFactory
                .createDefaultCollection(PluralAttribute.CollectionType.fromClass(att.getJavaType()));
        boolean elemTypeManaged = false;
        if (att instanceof PluralAttribute) {
            elemTypeManaged = uow.isTypeManaged(((PluralAttribute) att).getBindableJavaType());
        }
        for (Object item : mergedCol) {
            newValue.add(elemTypeManaged ? managedTypeMerger.getValueToSet(item, attributeDescriptor) : item);
        }
        extendModuleExtractionSignature(att, newValue);
        EntityPropertiesUtils.setFieldValue(att.getJavaField(), target, newValue);
    }

    private void extendModuleExtractionSignature(FieldSpecification<?, ?> att, Collection<?> value) {
        if (att instanceof TypesSpecification) {
            MetamodelUtils.checkForModuleSignatureExtension(value, uow.getMetamodel());
        }
    }
}

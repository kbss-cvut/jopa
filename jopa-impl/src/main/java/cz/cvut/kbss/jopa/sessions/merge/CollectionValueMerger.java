/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.sessions.merge;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.CollectionType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecord;
import cz.cvut.kbss.jopa.utils.CollectionFactory;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.MetamodelUtils;

import java.util.Collection;

class CollectionValueMerger implements ValueMerger {

    private final UnitOfWork uow;
    private final ManagedTypeValueMerger managedTypeMerger;

    CollectionValueMerger(UnitOfWork uow, ManagedTypeValueMerger managedTypeMerger) {
        this.uow = uow;
        this.managedTypeMerger = managedTypeMerger;
    }

    @Override
    public void mergeValue(Object target, ChangeRecord changeRecord, Descriptor attributeDescriptor) {
        final FieldSpecification<?, ?> att = changeRecord.getAttribute();
        final Collection<?> mergedCol = (Collection<?>) changeRecord.getNewValue();
        if (mergedCol == null) {
            EntityPropertiesUtils.setFieldValue(att.getJavaField(), target, null);
            return;
        }

        final Collection<Object> newValue = CollectionFactory
                .createDefaultCollection(CollectionType.fromClass(att.getJavaType()));
        boolean elemTypeManaged = isElementTypeManaged(att);
        for (Object item : mergedCol) {
            newValue.add(elemTypeManaged ? managedTypeMerger.getValueToSet(item, attributeDescriptor) : item);
        }
        extendModuleExtractionSignature(att, newValue);
        EntityPropertiesUtils.setFieldValue(att.getJavaField(), target, newValue);
    }

    private boolean isElementTypeManaged(FieldSpecification<?, ?> att) {
        return att instanceof PluralAttribute && uow.isEntityType(((PluralAttribute<?, ?, ?>) att).getBindableJavaType());
    }

    private void extendModuleExtractionSignature(FieldSpecification<?, ?> att, Collection<?> value) {
        if (att instanceof TypesSpecification) {
            MetamodelUtils.checkForModuleSignatureExtension(value, uow.getMetamodel());
        }
    }
}

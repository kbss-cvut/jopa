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
package cz.cvut.kbss.jopa.sessions.merge;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecord;

import java.util.Collection;
import java.util.Map;

public class DetachedValueMerger implements ValueMerger {

    private final UnitOfWork uow;

    private final ValueMerger defaultValueMerger;
    private final ManagedTypeValueMerger managedTypeMerger;
    private final CollectionValueMerger collectionMerger;
    private final MapValueMerger mapValueMerger;

    public DetachedValueMerger(UnitOfWork uow) {
        assert uow != null;
        this.uow = uow;
        this.defaultValueMerger = new DefaultValueMerger();
        this.managedTypeMerger = new ManagedTypeValueMerger(uow);
        this.collectionMerger = new CollectionValueMerger(uow, managedTypeMerger);
        this.mapValueMerger = new MapValueMerger();
    }

    @Override
    public void mergeValue(Object target, ChangeRecord changeRecord, Descriptor attributeDescriptor) {
        getMerger(changeRecord.getAttribute()).mergeValue(target, changeRecord, attributeDescriptor);
    }

    private ValueMerger getMerger(FieldSpecification<?, ?> att) {
        if (uow.isEntityType(att.getJavaType())) {
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

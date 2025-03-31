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
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecord;
import cz.cvut.kbss.jopa.utils.CollectionFactory;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.util.Map;

public class MapValueMerger implements ValueMerger {

    @Override
    public void mergeValue(Object target, ChangeRecord changeRecord, Descriptor attributeDescriptor) {
        final FieldSpecification<?, ?> att = changeRecord.getAttribute();
        final Map<?, ?> mergedMap = (Map<?, ?>) changeRecord.getNewValue();
        // This is a simplified version which will work only for the @Properties attributes
        // Bus since JOPA does not currently support any other use of Maps, it should be ok

        if (mergedMap == null) {
            EntityPropertiesUtils.setFieldValue(att.getJavaField(), target, null);
            return;
        }

        final Map<Object, Object> newMap = CollectionFactory.createDefaultMap();
        newMap.putAll(mergedMap);
        EntityPropertiesUtils.setFieldValue(att.getJavaField(), target, newMap);
    }
}

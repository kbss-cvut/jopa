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
import cz.cvut.kbss.jopa.sessions.ChangeRecord;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

public class DefaultValueMerger implements ValueMerger {

    public void mergeValue(FieldSpecification<?, ?> att, Object target, Object mergedValue) {
        EntityPropertiesUtils.setFieldValue(att.getJavaField(), target, mergedValue);
    }

    @Override
    public void mergeValue(Object target, ChangeRecord changeRecord, Descriptor attributeDescriptor) {
        EntityPropertiesUtils.setFieldValue(changeRecord.getAttribute().getJavaField(), target, changeRecord.getNewValue());
    }
}

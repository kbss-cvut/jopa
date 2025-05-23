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
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.proxy.change.ChangeTrackingIndirectMultilingualString;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.sessions.util.CloneConfiguration;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.lang.reflect.Field;

class MultilingualStringInstanceBuilder extends AbstractInstanceBuilder {

    MultilingualStringInstanceBuilder(CloneBuilder builder, UnitOfWork uow) {
        super(builder, uow);
    }

    @Override
    Object buildClone(Object cloneOwner, Field field, Object original, CloneConfiguration cloneConfiguration) {
        if (original == null) {
            return null;
        }
        assert original instanceof MultilingualString;
        MultilingualString orig = (MultilingualString) original;
        if (orig instanceof ChangeTrackingIndirectMultilingualString) {
            orig = ((ChangeTrackingIndirectMultilingualString) orig).unwrap();
        }
        return new ChangeTrackingIndirectMultilingualString(cloneOwner, field, uow, new MultilingualString(orig.getValue()));
    }

    @Override
    void mergeChanges(Field field, Object target, Object originalValue, Object cloneValue) {
        MultilingualString clone = (MultilingualString) cloneValue;
        if (clone instanceof ChangeTrackingIndirectMultilingualString) {
            clone = ((ChangeTrackingIndirectMultilingualString) clone).unwrap();
        }
        EntityPropertiesUtils
                .setFieldValue(field, target, clone != null ? new MultilingualString(clone.getValue()) : null);
    }

    @Override
    boolean populatesAttributes() {
        return true;
    }
}

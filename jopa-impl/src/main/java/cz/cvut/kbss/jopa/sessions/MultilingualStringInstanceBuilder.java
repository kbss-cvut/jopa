/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.adapters.IndirectMultilingualString;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.lang.reflect.Field;

class MultilingualStringInstanceBuilder extends AbstractInstanceBuilder {

    MultilingualStringInstanceBuilder(CloneBuilderImpl builder, UnitOfWorkImpl uow) {
        super(builder, uow);
    }

    @Override
    Object buildClone(Object cloneOwner, Field field, Object original, CloneConfiguration cloneConfiguration) {
        if (original == null) {
            return null;
        }
        assert original instanceof MultilingualString;
        MultilingualString orig = (MultilingualString) original;
        if (orig instanceof IndirectMultilingualString) {
            orig = ((IndirectMultilingualString) orig).unwrap();
        }
        return new IndirectMultilingualString(cloneOwner, field, uow, new MultilingualString(orig.getValue()));
    }

    @Override
    void mergeChanges(Field field, Object target, Object originalValue, Object cloneValue) {
        MultilingualString clone = (MultilingualString) cloneValue;
        if (clone instanceof IndirectMultilingualString) {
            clone = ((IndirectMultilingualString) clone).unwrap();
        }
        EntityPropertiesUtils
                .setFieldValue(field, target, clone != null ? new MultilingualString(clone.getValue()) : null);
    }

    @Override
    boolean populatesAttributes() {
        return true;
    }
}

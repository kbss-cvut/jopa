/**
 * Copyright (C) 2011 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.lang.reflect.Field;
import java.util.Date;

/**
 * @author ledvima1
 */
public class DateInstanceBuilder extends AbstractInstanceBuilder {

    public DateInstanceBuilder(CloneBuilderImpl builder, UnitOfWork uow) {
        super(builder, uow);
    }

    @Override
    Object buildClone(Object cloneOwner, Field field, Object original, Descriptor descriptor) {
        if (original == null) {
            return null;
        }
        final Date orig = (Date) original;
        return new Date(orig.getTime());
    }

    @Override
    void mergeChanges(Field field, Object target, Object originalValue, Object cloneValue) {
        EntityPropertiesUtils.setFieldValue(field, target, cloneValue);
    }

    @Override
    boolean populatesAttributes() {
        return true;
    }
}

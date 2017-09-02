/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.sessions.merge;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.utils.CollectionFactory;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.util.Map;

public class MapValueMerger implements ValueMerger {


    @Override
    public void mergeValue(FieldSpecification<?, ?> att, Object target, Object originalValue, Object mergedValue,
                           Descriptor attributeDescriptor) {
        final Map<?, ?> mergedMap = (Map<?, ?>) mergedValue;
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

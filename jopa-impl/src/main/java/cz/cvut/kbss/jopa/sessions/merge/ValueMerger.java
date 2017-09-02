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

@FunctionalInterface
public interface ValueMerger {

    /**
     * Merges new value of the specified field into the target object.
     *
     * @param att                 The attribute to merge
     * @param target              Target of the merge
     * @param originalValue       Original value of the field
     * @param mergedValue         The value to merge into the field
     * @param attributeDescriptor Specifies context of the merged attribute
     */
    void mergeValue(FieldSpecification<?, ?> att, Object target, Object originalValue, Object mergedValue,
                    Descriptor attributeDescriptor);
}

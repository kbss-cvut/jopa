/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model;

import java.util.*;

/**
 * Represents a map of references between types.
 * <p>
 * That is, it allows to get all entity classes which have attributes of the specified type.
 */
public class TypeReferenceMap {

    private final Map<Class<?>, Set<Class<?>>> referenceMap = new HashMap<>();

    /**
     * Registers reference relationships between the specified types.
     *
     * @param referencedType Type being referenced
     * @param referringType  Type referring to the referenced type
     */
    public void addReference(Class<?> referencedType, Class<?> referringType) {
        Objects.requireNonNull(referencedType);
        Objects.requireNonNull(referringType);
        if (!referenceMap.containsKey(referencedType)) {
            referenceMap.put(referencedType, new HashSet<>());
        }
        referenceMap.get(referencedType).add(referringType);
    }

    /**
     * Gets the set of entity classes containing an attribute of the specified type.
     *
     * @param referencedType Type being referenced for which referees should be returned
     * @return Set of referring classes, empty set if there are none
     */
    public Set<Class<?>> getReferringTypes(Class<?> referencedType) {
        return Collections.unmodifiableSet(referenceMap.getOrDefault(referencedType, Collections.emptySet()));
    }
}

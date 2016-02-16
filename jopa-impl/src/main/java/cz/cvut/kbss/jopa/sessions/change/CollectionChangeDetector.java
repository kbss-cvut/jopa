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
package cz.cvut.kbss.jopa.sessions.change;

import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.util.*;

/**
 * Checks for changes in a collection. By changes it is understood:
 * <pre>
 *     <ul>
 *         <li>Different number of elements,</li>
 *         <li>Different order of elements in a collection with deterministic ordering semantics (List, Queue,
 * <b>NOT!</b> Set)</li>
 *         <li>Different elements in the collection</li>
 *         <li>Changed elements in the collection (their attributes)</li>
 *     </ul>
 * </pre>
 */
class CollectionChangeDetector implements ChangeDetector {

    private final ChangeDetector changeDetector;

    private final ChangeManagerImpl changeManager;
    private final MetamodelProvider metamodelProvider;

    CollectionChangeDetector(ChangeDetector changeDetector, ChangeManagerImpl changeManager,
                             MetamodelProvider metamodelProvider) {
        this.changeDetector = changeDetector;
        this.changeManager = changeManager;
        this.metamodelProvider = metamodelProvider;
    }

    @Override
    public Changed hasChanges(Object clone, Object original) {
        assert clone != null;
        assert original != null;

        Collection<?> origCol = (Collection<?>) original;
        Collection<?> cloneCol = (Collection<?>) clone;
        if (origCol.size() != cloneCol.size()) {
            return Changed.TRUE;
        }
        if (origCol.isEmpty()) {
            return Changed.FALSE;
        }
        if (origCol instanceof Set) {
            return setChanged(cloneCol, origCol);
        } else {
            return orderedCollectionChanged(cloneCol, origCol);
        }
    }

    private Changed orderedCollectionChanged(Collection<?> clone, Collection<?> original) {
        Iterator<?> itOrig = original.iterator();
        Iterator<?> itClone = clone.iterator();
        boolean changes = false;
        while (itOrig.hasNext() && !changes) {
            Object cl = itClone.next();
            Object orig = itOrig.next();
            final Changed ch = changeDetector.hasChanges(orig, cl);
            switch (ch) {
                case TRUE:
                    changes = true;
                    break;
                case FALSE:
                    changes = false;
                    break;
                case UNDETERMINED:
                    changes = changeManager.hasChangesInternal(orig, cl);
                    break;
            }
        }
        return Changed.fromBoolean(changes);
    }

    /**
     * When checking for set changes, we have to make sure different element order does not mean a change.
     * <p>
     * Therefore, we first order the elements in a predictable way and then compare the elements.
     */
    private Changed setChanged(Collection<?> clone, Collection<?> original) {
        assert !clone.isEmpty();
        assert !original.isEmpty();

        final Class<?> elementType = clone.iterator().next().getClass();
        final List<?> cloneList = new ArrayList<>(clone);
        final Comparator<Object> comparator = getSetComparator(elementType);
        cloneList.sort(comparator);
        final List<?> originalList = new ArrayList<>(original);
        originalList.sort(comparator);
        return orderedCollectionChanged(cloneList, originalList);
    }

    /**
     * For managed types, the comparator uses identifier's hashCode, for all other types hashCode is used directly.
     */
    private Comparator<Object> getSetComparator(Class<?> elemType) {
        if (metamodelProvider.isTypeManaged(elemType)) {
            return (o1, o2) -> {
                final Object keyOne = EntityPropertiesUtils.getPrimaryKey(o1, metamodelProvider.getMetamodel());
                final Object keyTwo = EntityPropertiesUtils.getPrimaryKey(o2, metamodelProvider.getMetamodel());
                if (keyOne == null || keyTwo == null) {
                    return 0;
                }
                return keyOne.hashCode() - keyTwo.hashCode();
            };
        } else {
            return (o1, o2) -> o1.hashCode() - o2.hashCode();
        }
    }
}

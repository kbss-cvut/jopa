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
package cz.cvut.kbss.jopa.sessions.change;

import cz.cvut.kbss.jopa.sessions.MetamodelProvider;

import java.util.Collection;
import java.util.Map;

class ChangeDetectors implements ChangeDetector {

    private static final ChangeDetector MANAGED_TYPE_DETECTOR = new ManagedTypeChangeDetector();

    private final MetamodelProvider metamodelProvider;

    private final ChangeDetector mapChangeDetector;
    private final ChangeDetector collectionChangeDetector;

    ChangeDetectors(MetamodelProvider metamodelProvider, ChangeManagerImpl changeManager) {
        this.metamodelProvider = metamodelProvider;
        this.mapChangeDetector = new MapChangeDetector(this, changeManager);
        this.collectionChangeDetector = new CollectionChangeDetector(this, changeManager, metamodelProvider);
    }

    @Override
    public Changed hasChanges(Object clone, Object original) {
        if ((clone == null && original != null) || (clone != null && original == null)) {
            return Changed.TRUE;
        }
        if (clone == null) {
            return Changed.FALSE;
        }

        if (metamodelProvider.isTypeManaged(clone.getClass())) {
            return MANAGED_TYPE_DETECTOR.hasChanges(clone, original);
        } else if (clone instanceof Collection) {
            return collectionChangeDetector.hasChanges(clone, original);
        } else if (clone instanceof Map) {
            return mapChangeDetector.hasChanges(clone, original);
        }
        return Changed.fromBoolean(!clone.equals(original));
    }

    private static class ManagedTypeChangeDetector implements ChangeDetector {

        @Override
        public Changed hasChanges(Object clone, Object original) {
            return Changed.UNDETERMINED;
        }
    }
}

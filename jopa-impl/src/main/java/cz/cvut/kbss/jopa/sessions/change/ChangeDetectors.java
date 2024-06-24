/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.sessions.change;

import cz.cvut.kbss.jopa.sessions.MetamodelProvider;

import java.util.Collection;
import java.util.Map;

class ChangeDetectors implements ChangeDetector {

    private final MetamodelProvider metamodelProvider;

    private final ChangeDetector mapChangeDetector;
    private final ChangeDetector collectionChangeDetector;
    private final ChangeDetector managedTypeDetector;

    ChangeDetectors(MetamodelProvider metamodelProvider) {
        this.metamodelProvider = metamodelProvider;
        this.mapChangeDetector = new MapChangeDetector(this);
        this.collectionChangeDetector = new CollectionChangeDetector(this, metamodelProvider);
        this.managedTypeDetector = new ManagedTypeChangeDetector(metamodelProvider);
    }

    @Override
    public boolean hasChanges(Object clone, Object original) {
        if ((clone == null && original != null) || (clone != null && original == null)) {
            return isNonEmptyCollection(clone) && isNonEmptyCollection(original);
        }
        if (clone == null) {
            return false;
        }

        if (metamodelProvider.isEntityType(clone.getClass())) {
            return managedTypeDetector.hasChanges(clone, original);
        } else if (clone instanceof Collection) {
            return collectionChangeDetector.hasChanges(clone, original);
        } else if (clone instanceof Map) {
            return mapChangeDetector.hasChanges(clone, original);
        }
        return !clone.equals(original);
    }

    static boolean isNonEmptyCollection(Object instance) {
        return !(instance instanceof Collection) || !((Collection<?>) instance).isEmpty();
    }
}

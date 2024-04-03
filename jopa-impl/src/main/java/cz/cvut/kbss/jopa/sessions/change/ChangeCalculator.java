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
package cz.cvut.kbss.jopa.sessions.change;

import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * Calculates changes made on objects by comparing a clone with its registered original.
 */
public class ChangeCalculator {

    private static final Logger LOG = LoggerFactory.getLogger(ChangeCalculator.class);

    private final Map<Object, Object> visitedObjects;

    private final MetamodelProvider metamodelProvider;
    private final ChangeDetector changeDetector;

    public ChangeCalculator(MetamodelProvider metamodelProvider) {
        this.metamodelProvider = metamodelProvider;
        this.changeDetector = new ChangeDetectors(metamodelProvider);
        visitedObjects = new IdentityHashMap<>();
    }

    /**
     * Checks whether there are any changes to the clone.
     * <p>
     * It does an object value comparison, i.e. it compares each value of the clone against the original value and
     * returns true if a change is found.
     *
     * @param original The original object.
     * @param clone    The clone, whose changes we are looking for.
     * @return True if there is a change (at least one) or false, if the values are identical.
     */
    public boolean hasChanges(Object original, Object clone) {
        boolean res = hasChangesInternal(original, clone);
        visitedObjects.clear();
        return res;
    }

    /**
     * This method does the actual check for changes. It is wrapped in the public method since the IdentityMap for
     * visited objects has to be cleared after the whole check is done.
     *
     * @param original The original object.
     * @param clone    The clone that may have changed.
     * @return True if the clone is in different state than the original.
     */
    private boolean hasChangesInternal(Object original, Object clone) {
        if (clone == null && original == null) {
            return false;
        }
        if (clone == null || original == null) {
            return true;
        }
        if (visitedObjects.containsKey(clone)) {
            return false;
        }
        final Class<?> cls = clone.getClass();
        for (FieldSpecification<?, ?> fs : getFields(cls)) {
            final Field f = fs.getJavaField();
            final Object clVal = EntityPropertiesUtils.getFieldValue(f, clone);
            final Object origVal = EntityPropertiesUtils.getFieldValue(f, original);
            final boolean valueChanged = valueChanged(origVal, clVal);
            if (valueChanged) {
                return true;
            }
        }
        return false;
    }

    private <X> Set<FieldSpecification<? super X, ?>> getFields(Class<X> cls) {
        return metamodelProvider.getMetamodel().entity(cls).getFieldSpecifications();
    }

    private boolean valueChanged(Object orig, Object clone) {
        return changeDetector.hasChanges(clone, orig);
    }

    /**
     * Calculates the changes that happened to the clone object.
     * <p>
     * The changes are written into the {@link Change} passed in as argument.
     *
     * @param changeSet Contains references to the original and clone objects. Into this change set the changes should
     *                  be propagated
     * @return {@code true} if there were any changes, {@code false} otherwise
     * @throws NullPointerException If {@code changeSet} is {@code null}
     */
    public boolean calculateChanges(ObjectChangeSet changeSet) {
        return calculateChangesInternal(Objects.requireNonNull(changeSet));
    }

    /**
     * This internal method does the actual change calculation.
     * <p>
     * It compares every persistent attribute of the clone to the original value. If the values are different, a change
     * record is added to the change set.
     *
     * @param changeSet The change set where change records will be put in
     */
    private boolean calculateChangesInternal(ObjectChangeSet changeSet) {
        LOG.trace("Calculating changes for change set {}.", changeSet);
        Object original = changeSet.getOriginal();
        Object clone = changeSet.getClone();
        boolean changesFound = false;
        for (FieldSpecification<?, ?> fs : getFields(clone.getClass())) {
            final Field f = fs.getJavaField();
            Object clVal = EntityPropertiesUtils.getFieldValue(f, clone);
            Object origVal = EntityPropertiesUtils.getFieldValue(f, original);
            if (clVal == null && origVal == null) {
                continue;
            }
            boolean changed = valueChanged(origVal, clVal);
            if (changed) {
                changeSet.addChangeRecord(new ChangeRecord(fs, clVal));
                changesFound = true;
            }
        }
        return changesFound;
    }
}

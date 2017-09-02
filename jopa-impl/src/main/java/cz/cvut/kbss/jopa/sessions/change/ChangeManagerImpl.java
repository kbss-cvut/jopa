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

import cz.cvut.kbss.jopa.exceptions.OWLInferredAttributeModifiedException;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.ChangeManager;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

public class ChangeManagerImpl implements ChangeManager {

    private static final Logger LOG = LoggerFactory.getLogger(ChangeManagerImpl.class);

    private final Map<Object, Object> visitedObjects;

    private final MetamodelProvider metamodelProvider;
    private final ChangeDetector changeDetector;

    public ChangeManagerImpl(MetamodelProvider metamodelProvider) {
        this.metamodelProvider = metamodelProvider;
        this.changeDetector = new ChangeDetectors(metamodelProvider);
        visitedObjects = new IdentityHashMap<>();
    }

    @Override
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
    boolean hasChangesInternal(Object original, Object clone) {
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

    @Override
    public boolean calculateChanges(ObjectChangeSet changeSet) throws IllegalAccessException,
                                                                      IllegalArgumentException,
                                                                      OWLInferredAttributeModifiedException {
        return calculateChangesInternal(Objects.requireNonNull(changeSet));
    }

    /**
     * This internal method does the actual changes calculation. It compares every non-static attribute of the clone to
     * the original value. If the values are different, a change record is added into the change set.
     *
     * @param changeSet The change set where change records will be put in. It also contains reference to the clone and
     *                  original object.
     */
    private boolean calculateChangesInternal(ObjectChangeSet changeSet)
            throws IllegalArgumentException, IllegalAccessException {
        LOG.trace("Calculating changes for change set {}.", changeSet);
        Object original = changeSet.getChangedObject();
        Object clone = changeSet.getCloneObject();
        boolean changes = false;
        for (FieldSpecification<?, ?> fs : getFields(clone.getClass())) {
            final Field f = fs.getJavaField();
            Object clVal = EntityPropertiesUtils.getFieldValue(f, clone);
            Object origVal = EntityPropertiesUtils.getFieldValue(f, original);
            if (clVal == null && origVal == null) {
                continue;
            }
            boolean changed = valueChanged(origVal, clVal);
            if (changed) {
                changeSet.addChangeRecord(new ChangeRecordImpl(fs, clVal));
                changes = true;
            }
        }
        return changes;
    }
}

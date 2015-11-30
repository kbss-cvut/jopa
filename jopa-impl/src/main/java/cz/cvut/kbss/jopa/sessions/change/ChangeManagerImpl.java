package cz.cvut.kbss.jopa.sessions.change;

import cz.cvut.kbss.jopa.exceptions.OWLInferredAttributeModifiedException;
import cz.cvut.kbss.jopa.sessions.ChangeManager;
import cz.cvut.kbss.jopa.sessions.ChangeRecord;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.ErrorUtils;

import java.lang.reflect.Field;
import java.util.*;
import java.util.logging.Level;
import java.util.logging.Logger;

public class ChangeManagerImpl implements ChangeManager {

    private static final Logger LOG = Logger.getLogger(ChangeManagerImpl.class.getName());

    private final Map<Object, Object> visitedObjects;

    private final ChangeDetector changeDetector;

    public ChangeManagerImpl(MetamodelProvider metamodelProvider) {
        this.changeDetector = new ChangeDetectors(metamodelProvider, this);
        visitedObjects = new IdentityHashMap<>();
    }

    public boolean hasChanges(Object original, Object clone) {
        if (LOG.isLoggable(Level.FINEST)) {
            LOG.finest("Checking for changes...");
        }
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
        List<Field> fields = EntityPropertiesUtils.getAllFields(cls);
        Map<Object, Object> composedObjects = new HashMap<>();
        for (Field f : fields) {
            if (EntityPropertiesUtils.isFieldTransient(f)) {
                continue;
            }
            Object clVal = EntityPropertiesUtils.getFieldValue(f, clone);
            Object origVal = EntityPropertiesUtils.getFieldValue(f, original);
            final Changed ch = valueChanged(origVal, clVal);
            switch (ch) {
                case TRUE:
                    return true;
                case UNDETERMINED:
                    visitedObjects.put(clVal, clVal);
                    composedObjects.put(clVal, origVal);
                    break;
                default:
                    break;
            }
        }
        // First check all primitive values - performance, then do composed
        for (Object cl : composedObjects.keySet()) {
            if (hasChangesInternal(cl, composedObjects.get(cl))) {
                return true;
            }
        }
        return false;
    }

    Changed valueChanged(Object orig, Object clone) {
        return changeDetector.hasChanges(clone, orig);
    }

    public boolean calculateChanges(ObjectChangeSet changeSet) throws IllegalAccessException,
            IllegalArgumentException, OWLInferredAttributeModifiedException {
        Objects.requireNonNull(changeSet, ErrorUtils.constructNPXMessage("changeSet"));

        return calculateChangesInternal(changeSet);
    }

    /**
     * This internal method does the actual changes calculation. It compares every non-static attribute of the clone to
     * the original value. If the values are different, a change record is added into the change set.
     *
     * @param changeSet The change set where change records will be put in. It also contains reference to the clone and
     *                  original object.
     * @throws IllegalArgumentException
     * @throws IllegalAccessException
     * @throws OWLInferredAttributeModifiedException
     */
    protected boolean calculateChangesInternal(ObjectChangeSet changeSet)
            throws IllegalArgumentException, IllegalAccessException {
        if (LOG.isLoggable(Level.FINEST)) {
            LOG.finest("Calculating changes for change set " + changeSet);
        }
        Object original = changeSet.getChangedObject();
        Object clone = changeSet.getCloneObject();
        final List<Field> fields = EntityPropertiesUtils.getAllFields(clone.getClass());
        boolean changes = false;
        for (Field f : fields) {
            if (EntityPropertiesUtils.isFieldTransient(f)) {
                continue;
            }
            Object clVal = EntityPropertiesUtils.getFieldValue(f, clone);
            Object origVal = EntityPropertiesUtils.getFieldValue(f, original);
            ChangeRecord r = null;
            if (clVal == null && origVal == null) {
                continue;
            }
            final String attName = f.getName();
            Changed changed = valueChanged(origVal, clVal);
            switch (changed) {
                case FALSE:
                    continue;
                case TRUE:
                    r = new ChangeRecordImpl(attName, clVal);
                    changes = true;
                    break;
                case UNDETERMINED:
                    if (hasChanges(origVal, clVal)) {
                        changes = true;
                        r = new ChangeRecordImpl(attName, clVal);
                    } else {
                        continue;
                    }
            }
            changeSet.addChangeRecord(r);
        }
        return changes;
    }
}

package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.exceptions.OWLInferredAttributeModifiedException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.ErrorUtils;

import java.lang.reflect.Field;
import java.util.*;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

public class ChangeManagerImpl implements ChangeManager {

    private static final Logger LOG = Logger.getLogger(ChangeManagerImpl.class.getName());

    private final MetamodelProvider metamodelProvider;

    private final Map<Object, Object> visitedObjects;

    private static enum Changed {
        TRUE, FALSE, UNDETERMINED
    }

    public ChangeManagerImpl(MetamodelProvider metamodelProvider) {
        this.metamodelProvider = metamodelProvider;
        visitedObjects = new IdentityHashMap<>();
    }

    public boolean hasChanges(Object original, Object clone) {
        if (LOG.isLoggable(Level.FINEST)) {
            LOG.config("Checking for changes...");
        }
        boolean res = hasChangesInternal(original, clone);
        visitedObjects.clear();
        return res;
    }

    /**
     * This method does the actual check for changes. It is wrapped in the
     * public method since the IdentityMap for visited objects has to be cleared
     * after the whole check is done.
     *
     * @param original The original object.
     * @param clone    The clone that may have changed.
     * @return True if the clone is in different state than the original.
     */
    protected boolean hasChangesInternal(Object original, Object clone) {
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
        Iterator<Field> it = fields.iterator();
        try {
            while (it.hasNext()) {
                Field f = it.next();
                if (!f.isAccessible()) {
                    f.setAccessible(true);
                }
                Object clVal = f.get(clone);
                Object origVal = f.get(original);
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
        } catch (IllegalAccessException e) {
            throw new OWLPersistenceException(
                    "Exception caught when trying to check changes on entities.", e);
        }
        return false;
    }

    private Changed valueChanged(Object orig, Object clone) {
        if ((clone == null && orig != null) || (clone != null && orig == null)) {
            return Changed.TRUE;
        }
        if (clone == null) {
            return Changed.FALSE;
        }
        boolean changes;
        if (metamodelProvider.isTypeManaged(clone.getClass())) {
            return Changed.UNDETERMINED;
        } else if (clone instanceof Collection) {
            changes = hasCollectionChanged(clone, orig);
        } else if (clone instanceof Map) {
            changes = hasMapChanges(clone, orig);
        } else {
            changes = !clone.equals(orig);
        }
        return changes ? Changed.TRUE : Changed.FALSE;
    }

    /**
     * This method checks for changes in collections. A change may be different
     * size - removed or added element, or different elements or even different
     * order of elements.
     *
     * @param clone    The cloned collection.
     * @param original The original collection.
     * @return True if the clone collection contains any modifications compared
     * to the original collection.
     */
    private boolean hasCollectionChanged(Object clone, Object original) {
        boolean hasChanged = false;
        Collection<?> origCol = (Collection<?>) original;
        Collection<?> cloneCol = (Collection<?>) clone;
        if (origCol.size() != cloneCol.size()) {
            return true;
        }
        Iterator<?> itOrig = origCol.iterator();
        Iterator<?> itClone = cloneCol.iterator();
        while (itOrig.hasNext() && itClone.hasNext() && !hasChanged) {
            Object cl = itClone.next();
            Object orig = itOrig.next();
            final Changed ch = valueChanged(orig, cl);
            switch (ch) {
                case TRUE:
                    hasChanged = true;
                    break;
                case FALSE:
                    hasChanged = false;
                    break;
                case UNDETERMINED:
                    hasChanged = hasChangesInternal(orig, cl);
                    break;
            }
        }
        return hasChanged;
    }

    private boolean hasMapChanges(Object clone, Object original) {
        final Map<?, ?> cl = (Map<?, ?>) clone;
        final Map<?, ?> orig = (Map<?, ?>) original;
        if (orig.size() != cl.size()) {
            return true;
        }
        for (Entry<?, ?> e : orig.entrySet()) {
            final Object origKey = e.getKey();
            if (!cl.containsKey(origKey)) {
                return true;
            }
            // TODO Maybe we should check also for key changes
            final Object origVal = e.getValue();
            Object clVal = cl.get(origKey);

            final Changed ch = valueChanged(origVal, clVal);
            switch (ch) {
                case TRUE:
                    return true;
                case FALSE:
                    return false;
                case UNDETERMINED:
                    return hasChangesInternal(origVal, clVal);
            }
        }
        return false;
    }

    public boolean calculateChanges(ObjectChangeSet changeSet) throws IllegalAccessException,
            IllegalArgumentException, OWLInferredAttributeModifiedException {
        Objects.requireNonNull(changeSet, ErrorUtils.constructNPXMessage("changeSet"));

        return calculateChangesInternal(changeSet);
    }

    /**
     * This internal method does the actual changes calculation. It compares
     * every non-static attribute of the clone to the original value. If the
     * values are different, a change record is added into the change set.
     *
     * @param changeSet The change set where change records will be put in. It also
     *                  contains reference to the clone and original object.
     * @throws IllegalArgumentException
     * @throws IllegalAccessException
     * @throws OWLInferredAttributeModifiedException
     */
    protected boolean calculateChangesInternal(ObjectChangeSet changeSet)
            throws IllegalArgumentException, IllegalAccessException {
        if (LOG.isLoggable(Level.FINER)) {
            LOG.finer("Calculating changes for change set " + changeSet);
        }
        Object original = changeSet.getChangedObject();
        Object clone = changeSet.getCloneObject();
        final List<Field> fields = EntityPropertiesUtils.getAllFields(clone.getClass());
        boolean changes = false;
        for (Field f : fields) {
            if (!f.isAccessible()) {
                f.setAccessible(true);
            }
            Object clVal = f.get(clone);
            Object origVal = f.get(original);
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

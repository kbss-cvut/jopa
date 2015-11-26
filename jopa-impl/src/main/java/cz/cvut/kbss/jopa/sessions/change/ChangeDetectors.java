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

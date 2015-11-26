package cz.cvut.kbss.jopa.sessions.change;

import java.util.Iterator;
import java.util.Map;

class MapChangeDetector implements ChangeDetector {

    private final ChangeDetector changeDetector;
    private final ChangeManagerImpl changeManager;

    public MapChangeDetector(ChangeDetector changeDetector, ChangeManagerImpl changeManager) {
        this.changeDetector = changeDetector;
        this.changeManager = changeManager;
    }

    @Override
    public Changed hasChanges(Object clone, Object original) {
        assert clone != null;
        assert original != null;

        final Map<?, ?> cl = (Map<?, ?>) clone;
        final Map<?, ?> orig = (Map<?, ?>) original;
        if (orig.size() != cl.size()) {
            return Changed.TRUE;
        }
        boolean changes = false;
        final Iterator<?> it = orig.keySet().iterator();
        while (it.hasNext() && !changes) {
            final Object origKey = it.next();
            if (!cl.containsKey(origKey)) {
                return Changed.TRUE;
            }
            // TODO Maybe we should check also for key changes
            final Object origVal = orig.get(origKey);
            Object clVal = cl.get(origKey);

            final Changed ch = changeDetector.hasChanges(origVal, clVal);
            switch (ch) {
                case TRUE:
                    return ch;
                case FALSE:
                    break;
                case UNDETERMINED:
                    changes = changeManager.hasChangesInternal(origVal, clVal);
            }
        }
        return Changed.fromBoolean(changes);
    }
}

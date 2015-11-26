package cz.cvut.kbss.jopa.sessions.change;

/**
 * Detects changes in objects.
 */
interface ChangeDetector {

    /**
     * Finds out whether an object has changed.
     *
     * @param clone    The possibly changed clone
     * @param original Original object
     * @return Change status
     */
    Changed hasChanges(Object clone, Object original);
}

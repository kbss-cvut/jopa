package cz.cvut.kbss.jopa.sessions.change;

/**
 * Whether an object has changed.
 */
enum Changed {
    TRUE, FALSE, UNDETERMINED;

    static Changed fromBoolean(boolean value) {
        return value ? TRUE : FALSE;
    }
}

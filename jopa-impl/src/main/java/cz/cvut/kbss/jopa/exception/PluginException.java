package cz.cvut.kbss.jopa.exception;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;

/**
 * Exception thrown when an error occurs in a plugin.
 */
public class PluginException extends OWLPersistenceException {

    public PluginException(String message, Throwable cause) {
        super(message, cause);
    }
}

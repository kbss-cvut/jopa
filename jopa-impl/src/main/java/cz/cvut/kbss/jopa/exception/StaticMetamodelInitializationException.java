package cz.cvut.kbss.jopa.exception;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;

/**
 * Indicates that the static metamodel could not be initialized.
 */
public class StaticMetamodelInitializationException extends OWLPersistenceException {

    public StaticMetamodelInitializationException(String message) {
        super(message);
    }

    public StaticMetamodelInitializationException(String message, Throwable cause) {
        super(message, cause);
    }
}

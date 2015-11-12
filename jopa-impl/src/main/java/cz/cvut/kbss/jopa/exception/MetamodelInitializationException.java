package cz.cvut.kbss.jopa.exception;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;

/**
 * Indicates an error when building application persistence metamodel.
 */
public class MetamodelInitializationException extends OWLPersistenceException {

    public MetamodelInitializationException() {
    }

    public MetamodelInitializationException(String message, Throwable cause) {
        super(message, cause);
    }

    public MetamodelInitializationException(String message) {
        super(message);
    }

    public MetamodelInitializationException(Throwable cause) {
        super(cause);
    }
}

package cz.cvut.kbss.jopa.exception;

/**
 * Indicates that an enum mapping is not valid.
 */
public class InvalidEnumMappingException extends MetamodelInitializationException {

    public InvalidEnumMappingException(String message) {
        super(message);
    }
}

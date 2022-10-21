package cz.cvut.kbss.jopa.exception;

/**
 * Indicates that an issue has occurred with user-defined attribute converters.
 */
public class InvalidConverterException extends MetamodelInitializationException {

    public InvalidConverterException(String message) {
        super(message);
    }

    public InvalidConverterException(String message, Throwable cause) {
        super(message, cause);
    }
}

package cz.cvut.kbss.jopa.datatype.exception;

/**
 * Indicates that the mapping has failed.
 * <p>
 * For example, if the literal value is not valid (e.g., an integer literal that cannot be parsed to Java {@link Integer}).
 */
public class DatatypeMappingException extends RuntimeException {

    public DatatypeMappingException(String message) {
        super(message);
    }

    public DatatypeMappingException(String message, Throwable cause) {
        super(message, cause);
    }
}

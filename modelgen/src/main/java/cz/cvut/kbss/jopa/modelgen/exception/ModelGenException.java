package cz.cvut.kbss.jopa.modelgen.exception;

/**
 * General exception for the metamodel generator.
 */
public class ModelGenException extends RuntimeException {

    public ModelGenException(String message) {
        super(message);
    }

    public ModelGenException(String message, Throwable cause) {
        super(message, cause);
    }
}

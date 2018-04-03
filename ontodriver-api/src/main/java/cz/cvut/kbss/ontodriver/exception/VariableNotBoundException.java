package cz.cvut.kbss.ontodriver.exception;

/**
 * Thrown when an attempt is made to extract value of a variable which is not bound by the {@link cz.cvut.kbss.ontodriver.ResultSet}.
 * <p>
 * This can happen for example when an OPTIONAL operator is used, so the variable is present in the result set, but
 * may not be bound by the current row.
 */
public class VariableNotBoundException extends RuntimeException {

    public VariableNotBoundException() {
    }

    public VariableNotBoundException(String message) {
        super(message);
    }
}

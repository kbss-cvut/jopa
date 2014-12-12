package cz.cvut.kbss.jopa.accessors;

/**
 * Created by ledvima1 on 12.12.14.
 */
public class DataSourceCreationException extends RuntimeException {
    public DataSourceCreationException() {
    }

    public DataSourceCreationException(String message) {
        super(message);
    }

    public DataSourceCreationException(String message, Throwable cause) {
        super(message, cause);
    }

    public DataSourceCreationException(Throwable cause) {
        super(cause);
    }
}

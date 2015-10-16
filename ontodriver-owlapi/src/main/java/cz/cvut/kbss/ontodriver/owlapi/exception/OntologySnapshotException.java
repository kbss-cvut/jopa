package cz.cvut.kbss.ontodriver.owlapi.exception;

/**
 * Created by ledvima1 on 26.2.15.
 */
public class OntologySnapshotException extends RuntimeException {

    public OntologySnapshotException() {
    }

    public OntologySnapshotException(String message) {
        super(message);
    }

    public OntologySnapshotException(String message, Throwable cause) {
        super(message, cause);
    }

    public OntologySnapshotException(Throwable cause) {
        super(cause);
    }
}

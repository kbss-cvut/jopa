package cz.cvut.kbss.ontodriver;

/**
 * This exception is thrown when metamodel is not set for a Connection.
 * 
 * @author kidney
 * 
 */
public class MetamodelNotSetException extends RuntimeException {

	private static final long serialVersionUID = -1639928487851527873L;

	public MetamodelNotSetException() {
	}

	public MetamodelNotSetException(String message) {
		super(message);
	}

	public MetamodelNotSetException(Throwable cause) {
		super(cause);
	}

	public MetamodelNotSetException(String message, Throwable cause) {
		super(message, cause);
	}

	public MetamodelNotSetException(String message, Throwable cause,
			boolean enableSuppression, boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}

}

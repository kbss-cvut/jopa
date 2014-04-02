package cz.cvut.kbss.jopa.utils;

public final class ErrorUtils {

	/**
	 * Error message stating that an argument cannot be null. </p>
	 * 
	 * This message contains a placeholder {@code arg}, which should be replaced
	 * with the actual argument name.
	 */
	private static final String ARGUMENT_NULL = "Argument '$arg$' cannot be null.";

	private static final String PLACEHOLDER = "$arg$";

	private ErrorUtils() {
		throw new AssertionError();
	}

	/**
	 * Constructs NullPointerException error message, which states than an
	 * argument with the specified name cannot be null.
	 * 
	 * @param argName
	 *            Argument name
	 * @return Error message
	 */
	public static String constructNPXMessage(String argName) {
		return ARGUMENT_NULL.replace(PLACEHOLDER, argName);
	}
}

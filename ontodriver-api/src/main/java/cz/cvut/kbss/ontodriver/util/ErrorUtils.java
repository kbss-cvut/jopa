package cz.cvut.kbss.ontodriver.util;

public class ErrorUtils {

    /**
     * Error message stating that an argument cannot be null. </p>
     * <p>
     * This message contains a placeholder {@code arg}, which should be replaced with the actual argument name.
     */
    private static final String ARGUMENT_NULL = "Argument '$arg$' cannot be null.";

    private static final String PLACEHOLDER = "$arg$";

    private ErrorUtils() {
        throw new AssertionError();
    }

    /**
     * Creates message for NullPointerException, specifying name of method argument which was null.
     * <p>
     * The message has the following form: 'Argument argName cannot be null.'
     *
     * @param argName Name of the argument
     * @return String message for NPX exception
     */
    public static String npxMessage(String argName) {
        return ARGUMENT_NULL.replace(PLACEHOLDER, argName);
    }
}

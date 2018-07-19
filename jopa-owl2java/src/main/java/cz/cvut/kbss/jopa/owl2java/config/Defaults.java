package cz.cvut.kbss.jopa.owl2java.config;

import cz.cvut.kbss.jopa.owl2java.Constants;
import cz.cvut.kbss.jopa.owl2java.cli.Option;
import cz.cvut.kbss.jopa.owl2java.cli.PropertiesType;

/**
 * Default values of {@link Option}s.
 */
public class Defaults {

    /**
     * @see Option#WITH_IRIS
     */
    public static boolean WITH_IRIS = false;

    /**
     * @see Option#TARGET_DIR
     */
    public static String TARGET_DIR = "";

    /**
     * @see Option#PACKAGE
     */
    public static String PACKAGE = "generated";

    /**
     * @see Option#WHOLE_ONTOLOGY_AS_IC
     */
    public static boolean WHOLE_ONTOLOGY_AS_IC = false;

    /**
     * @see Option#IGNORE_FAILED_IMPORTS
     */
    public static boolean IGNORE_FAILED_IMPORTS = false;

    /**
     * @see Option#JAVA_CLASSNAME_ANNOTATION
     */
    public static String JAVA_CLASSNAME_ANNOTATION = Constants.P_CLASS_NAME;

    /**
     * @see Option#PROPERTIES_TYPE
     */
    public static String PROPERTIES_TYPE = PropertiesType.string.name();

    /**
     * @see Option#GENERATE_JAVADOC_FROM_COMMENT
     */
    public static boolean GENERATE_JAVADOC_FROM_COMMENT = true;

    private Defaults() {
        throw new AssertionError();
    }
}

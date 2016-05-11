package cz.cvut.kbss.jopa.owl2java;

public class Constants {

    private Constants() {
        throw new AssertionError();
    }

    /**
     * Name of the class containing generated vocabulary.
     */
    public static final String VOCABULARY_CLASS = "Vocabulary";

    /**
     * Package into which the model is generated.
     */
    public static final String MODEL_PACKAGE = "model";

    /**
     * Java package name separator.
     */
    public static final char PACKAGE_SEPARATOR = '.';
}

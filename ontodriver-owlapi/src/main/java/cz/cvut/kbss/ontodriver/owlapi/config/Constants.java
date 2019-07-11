package cz.cvut.kbss.ontodriver.owlapi.config;

public class Constants {

    /**
     * Default language to use when an {@link cz.cvut.kbss.ontodriver.model.Assertion} does not specify a language.
     * <p>
     * The {@code null} value ensures that strings will be saved as xsd:string and loaded with any language tag (or
     * without a language tag at all).
     */
    public static final String DEFAULT_LANGUAGE = null;

    private Constants() {
        throw new AssertionError();
    }
}

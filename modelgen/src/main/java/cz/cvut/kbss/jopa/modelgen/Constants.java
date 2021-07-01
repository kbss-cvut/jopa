package cz.cvut.kbss.jopa.modelgen;

public class Constants {

    public static class Options {

        public static final String DEBUG = "debug";
        public static final String ADD_GENERATION_DATE = "addGenerationDate";
        public static final String ADD_GENERATED_ANNOTATION = "addGeneratedAnnotation";

        private Options() {
            throw new AssertionError();
        }
    }

    /**
     * Tool version.
     */
    public static final String VERSION = "$VERSION$";

    private Constants() {
        throw new AssertionError();
    }
}

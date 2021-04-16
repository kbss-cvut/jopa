package cz.cvut.kbss.jopa.modelgen;

import com.sun.xml.internal.ws.policy.spi.AssertionCreationException;

public class Constants {

    public static class Options {

        public static final String DEBUG = "debug";
        public static final String ADD_GENERATION_DATE = "addGenerationDate";
        public static final String ADD_GENERATED_ANNOTATION = "addGeneratedAnnotation";

        private Options() {
            throw new AssertionError();
        }
    }

    private Constants() {
        throw new AssertionError();
    }
}

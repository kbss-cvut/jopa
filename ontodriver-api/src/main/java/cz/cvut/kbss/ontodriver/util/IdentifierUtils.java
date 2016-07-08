package cz.cvut.kbss.ontodriver.util;

import java.net.URI;
import java.util.Random;

/**
 * Utility for automatic identifier generation.
 */
public class IdentifierUtils {

    private static final Random RANDOM = new Random();

    /**
     * Generates a (pseudo) random identifier based on the specified class URI.
     * <p>
     * The identifier consists of the class URI and then contains the string '_instance' and a random integer to ensure
     * uniqueness.
     *
     * @param classUri Class URI used as identifier base
     * @return Generated identifier
     */
    public static URI generateIdentifier(URI classUri) {
        if (classUri.getFragment() != null) {
            return URI.create(classUri.toString() + "_instance" + RANDOM.nextInt());
        } else {
            String base = classUri.toString();
            if (base.endsWith("/")) {
                return URI.create(base + "_instance" + RANDOM.nextInt());
            } else {
                return URI.create(base + "#instance" + RANDOM.nextInt());
            }
        }
    }
}

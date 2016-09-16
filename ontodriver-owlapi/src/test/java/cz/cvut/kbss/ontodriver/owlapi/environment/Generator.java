package cz.cvut.kbss.ontodriver.owlapi.environment;

import java.net.URI;
import java.util.Random;

public class Generator {

    private static final Random RANDOM = new Random();

    private static final String URI_BASE = "http://krizik.felk.cvut.cz/ontologies/jopa/ontodriver/";

    private Generator() {
        throw new AssertionError();
    }

    /**
     * Generates a (pseudo) random URI
     *
     * @return Random URI
     */
    public static URI generateUri() {
        return URI.create(URI_BASE + RANDOM.nextInt());
    }

    public static int randomInt(int max) {
        return RANDOM.nextInt(max);
    }
}

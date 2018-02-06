package cz.cvut.kbss.ontodriver.jena.environment;

import java.net.URI;
import java.util.Random;

public class Generator {

    private static final String URI_BASE = "http://onto.fel.cvut.cz/ontologies/ontodriver/jena/";

    private static final Random RANDOM = new Random();

    public static URI generateUri() {
        return URI.create(URI_BASE + "instance" + Integer.toString(RANDOM.nextInt()));
    }

    public static int randomInt() {
        return RANDOM.nextInt();
    }

    public static boolean randomBoolean() {
        return RANDOM.nextBoolean();
    }
}

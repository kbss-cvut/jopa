package cz.cvut.kbss.jopa.owl2java.environment;

import java.net.URI;
import java.util.Random;

public class Generator {


    private static final Random RANDOM = new Random();

    public static final String IRI_BASE = "https://onto.fel.cvut.cz/ontologies/jopa/owl2java/";

    public static URI generateUri() {
        return URI.create(IRI_BASE + RANDOM.nextInt());
    }
}

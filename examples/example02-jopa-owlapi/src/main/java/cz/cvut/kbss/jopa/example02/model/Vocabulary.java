package cz.cvut.kbss.jopa.example02.model;

public class Vocabulary {

    private Vocabulary() {
        throw new AssertionError();
    }

    public static final String BASE_URI = "http://krizik.felk.cvut.cz/ontologies/jopa/example02#";

    public static final String JEDI = BASE_URI + "Jedi";
    public static final String HAS_CHILD = BASE_URI + "hasChild";
    public static final String HAS_FATHER = BASE_URI + "hasFather";

    public static final String FIRST_NAME = "http://xmlns.com/foaf/0.1/firstName";
    public static final String LAST_NAME = "http://xmlns.com/foaf/0.1/lastName";
    public static final String NICKNAME = "http://xmlns.com/foaf/0.1/nick";
}

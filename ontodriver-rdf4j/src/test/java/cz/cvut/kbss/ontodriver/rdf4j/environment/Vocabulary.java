package cz.cvut.kbss.ontodriver.rdf4j.environment;

public class Vocabulary {

    public static final String CLASS_IRI_BASE = "https://onto.fel.cvut.cz/ontologies/jopa/classes/";
    public static final String PROPERTY_IRI_BASE = "https://onto.fel.cvut.cz/ontologies/jopa/attributes/";
    public static final String INDIVIDUAL_IRI_BASE = "https://onto.fel.cvut.cz/ontologies/jopa/entities/";

    private Vocabulary() {
        throw new AssertionError();
    }
}

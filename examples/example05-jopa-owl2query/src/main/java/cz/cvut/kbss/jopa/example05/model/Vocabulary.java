package cz.cvut.kbss.jopa.example05.model;

/**
 * It is usually better to have OWL properties and classes defined in a vocabulary file, so that we can for example
 * reference them in queries (see {@link cz.cvut.kbss.jopa.example05.dao.SuperheroDao#findAllAssociates(cz.cvut.kbss.jopa.example05.model.Superhero)}).
 */
public class Vocabulary {

    private Vocabulary() {
        throw new AssertionError();
    }

    public static final String Superhero = "http://krizik.felk.cvut.cz/ontologies/jopa/example05#Superhero";

    public static final String p_firstName = "http://krizik.felk.cvut.cz/ontologies/jopa/example05#firstName";
    public static final String p_lastName = "http://krizik.felk.cvut.cz/ontologies/jopa/example05#lastName";
    public static final String p_nickname = "http://krizik.felk.cvut.cz/ontologies/jopa/example05#nickname";
    public static final String p_knows = "http://krizik.felk.cvut.cz/ontologies/jopa/example05#knows";

    public static final String p_goodGuy = "http://krizik.felk.cvut.cz/ontologies/jopa/example05#isGoodGuy";
}

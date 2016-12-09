package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.ontodriver.model.Assertion;

public class Vocabulary {

    private static final String ATTRIBUTE_BASE = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#";
    private static final String CLASS_BASE = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#";

    public static final String c_OwlClassM = CLASS_BASE + "OWLClassM";

    public static final String p_m_booleanAttribute = ATTRIBUTE_BASE + "m-booleanAttribute";
    public static final String p_m_intAttribute = ATTRIBUTE_BASE + "m-intAttribute";
    public static final String p_m_longAttribute = ATTRIBUTE_BASE + "m-longAttribute";
    public static final String p_m_doubleAttribute = ATTRIBUTE_BASE + "m-doubleAttribute";
    public static final String p_m_dateAttribute = ATTRIBUTE_BASE + "m-dateAttribute";
    public static final String p_m_enumAttribute = ATTRIBUTE_BASE + "m-enumAttribute";
    public static final String p_m_IntegerSet = ATTRIBUTE_BASE + "m-pluralIntAttribute";

    private Vocabulary() {
        throw new AssertionError();
    }
}

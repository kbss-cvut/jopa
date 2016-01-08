package cz.cvut.kbss.jopa.example03.model;

public class Vocabulary {

    private Vocabulary() {
        throw new AssertionError();
    }

    public static final String BASE_URI = "http://krizik.felk.cvut.cz/ontologies/jopa/example03#";

    public static final String Operator = BASE_URI + "Operator";
    public static final String Aircraft = BASE_URI + "Aircraft";
    public static final String Flight = BASE_URI + "Flight";
    public static final String Accident = BASE_URI + "Accident";

    public static final String p_name = BASE_URI + "name";
    public static final String p_code = BASE_URI + "code";

    public static final String p_registration = BASE_URI + "registration";
    public static final String p_stateOfRegistry = BASE_URI + "registryState";
    public static final String p_manufacturer = BASE_URI + "manufacturer";
    public static final String p_type = BASE_URI + "type";

    public static final String p_hasOperator = BASE_URI + "hasOperator";
    public static final String p_hasPlane = BASE_URI + "hasPlane";
    public static final String p_flightNum = BASE_URI + "flightNumber";

    public static final String p_hasFlight = BASE_URI + "hasAffectedFlight";
    public static final String p_cause = BASE_URI + "cause";
}

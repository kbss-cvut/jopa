package cz.cvut.kbss.ontodriver.model;

/**
 * Indicates whether to consider inferred data and under which conditions.
 */
public enum InferenceMode {

    /**
     * Consider only explicit (asserted) data.
     */
    EXPLICIT,
    /**
     * Consider only inferred data.
     */
    INFERRED,
    /**
     * Consider both explicit (asserted) and inferred data.
     */
    EXPLICIT_AND_INFERRED
}

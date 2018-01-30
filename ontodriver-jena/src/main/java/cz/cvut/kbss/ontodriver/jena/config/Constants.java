package cz.cvut.kbss.ontodriver.jena.config;

/**
 * Constants and default values for the Jena driver.
 */
public class Constants {

    /**
     * Default transaction isolation strategy used by the driver.
     *
     * @see JenaOntoDriverProperties#JENA_ISOLATION_STRATEGY
     */
    public static final String DEFAULT_ISOLATION_STRATEGY = JenaOntoDriverProperties.READ_COMMITTED;

    private Constants() {
        throw new AssertionError();
    }
}

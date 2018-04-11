package cz.cvut.kbss.ontodriver.jena.config;

public class JenaOntoDriverProperties {

    /**
     * Represents parameter specify isolation strategy to be used.
     * <p>
     * Possible values:
     * <ul>
     * <li>{@link #READ_COMMITTED}</li>
     * <li>{@link #SNAPSHOT}</li>
     * </ul>
     */
    public static final String JENA_ISOLATION_STRATEGY = "cz.cvut.kbss.ontodriver.jena.isolation";

    /**
     * Default storage access isolation strategy.
     * <p>
     * Represents situation where transactions keep a list of their changes, but the read from a shared connector,
     * meaning when a transaction commits changes, other transactions immediately see them.
     *
     * @see #JENA_ISOLATION_STRATEGY
     */
    public static final String READ_COMMITTED = "read-committed";

    /**
     * Storage access isolation strategy.
     * <p>
     * Represents situations where each transactional acquires a complete snapshot of the repository and works with it.
     *
     * @see #JENA_ISOLATION_STRATEGY
     */
    public static final String SNAPSHOT = "snapshot";

    /**
     * Represents parameter specifying type of storage to be used by this driver.
     * <p>
     * Possible values are:
     * <ul>
     * <li>{@link #FILE}</li>
     * <li>{@link #IN_MEMORY}</li>
     * <li>{@link #TDB}</li>
     * <li>{@link #SDB} - SDB storage is currently not supported (and probably won't be, since SDB development has stopped)</li>
     * </ul>
     */
    public static final String JENA_STORAGE_TYPE = "cz.cvut.kbss.ontodriver.jena.storage";

    /**
     * Plain file storage.
     *
     * @see #JENA_STORAGE_TYPE
     */
    public static final String FILE = "file";

    /**
     * In-memory storage.
     *
     * @see #JENA_STORAGE_TYPE
     */
    public static final String IN_MEMORY = "in-memory";

    /**
     * Jena TDB storage.
     *
     * @see #JENA_STORAGE_TYPE
     */
    public static final String TDB = "tdb";

    /**
     * Jena SDB storage.
     *
     * @see #JENA_STORAGE_TYPE
     */
    public static final String SDB = "sdb";

    /**
     * Parameter specifying whether the default graph should be treated as union of all the named graphs + the default graph.
     * <p>
     * Default graph being a union of all graphs is typical for storages like RDF4J, GraphDB, Virtuoso (querying without
     * graph queries across all graphs).
     * <p>
     * This parameter influences only querying/reading, it has no effect on write operations, which work with the default graph
     * as is in Jena.
     */
    public static final String JENA_TREAT_DEFAULT_GRAPH_AS_UNION = "cz.cvut.kbss.ontodriver.jena.default_graph_as_union";

    private JenaOntoDriverProperties() {
        throw new AssertionError();
    }
}

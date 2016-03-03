package cz.cvut.kbss.ontodriver.sesame.config;

public abstract class SesameOntoDriverProperties {

    private SesameOntoDriverProperties() {
        throw new AssertionError();
    }

    /**
     * Specifies whether a in-memory storage should be used for local Sesame
     * repositories. </p>
     *
     * When set to true, any local Sesame repositories that are created by the
     * driver are created as only MemoryStores without any persistent backend.
     * Repositories accessed over the Internet or already existing locally are
     * not affected by this setting. </p>
     *
     * {@code Boolean} value expected, default is false.
     */
    public static final String SESAME_USE_VOLATILE_STORAGE = "cz.cvut.kbss.ontodriver.sesame.use-volatile-storage";

    /**
     * Specifies whether Sesame inference (RDFS, forward chaining) should be
     * used. </p>
     *
     * Note that this setting applies only to local storages (in memory or
     * native), remote storages use their own inference settings. </p>
     *
     * {@code Boolean} value expected, default is false.
     */
    public static final String SESAME_USE_INFERENCE = "cz.cvut.kbss.ontodriver.sesame.use-inference";
}

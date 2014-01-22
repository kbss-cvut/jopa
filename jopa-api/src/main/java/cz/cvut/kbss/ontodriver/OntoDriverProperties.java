package cz.cvut.kbss.ontodriver;

public final class OntoDriverProperties {

	/**
	 * Property for setting default auto-commit strategy for connections.
	 */
	public static final String CONNECTION_AUTO_COMMIT = "cz.cvut.kbss.ontodriver.connection-auto-commit";
	/**
	 * Reasoner factory class property.
	 */
	public static final String OWLAPI_REASONER_FACTORY_CLASS = "cz.cvut.jopa.reasonerFactoryClass";
	/**
	 * Ontology language property.
	 */
	public static final String ONTOLOGY_LANGUAGE = "cz.cvut.jopa.lang";

	/**
	 * This setting tells the driver whether to use the transactional ontology
	 * for retrieving entities and answering queries. </p>
	 * 
	 * If so, uncommitted changes made during transaction will be included in
	 * query evaluation, entity retrieval etc. Otherwise the driver will use the
	 * ontology as it was when the transaction was started and uncommitted
	 * changes will not be visible until commit.
	 */
	public static final String USE_TRANSACTIONAL_ONTOLOGY = "cz.cvut.kbss.ontodriver.use-transactional-onto";

	/**
	 * Property for setting factory for OWL API based modules and connectors.
	 * </p>
	 * 
	 * If not specified, default factories are used.
	 */
	public static final String OWLAPI_DRIVER_FACTORY = "cz.cvut.kbss.ontodriver.owlapi-factory";

	/**
	 * Property for setting factory for Jena based modules and connectors. </p>
	 * 
	 * If not specified, default factories are used.
	 */
	public static final String JENA_DRIVER_FACTORY = "cz.cvut.kbss.ontodriver.jena-factory";

	/**
	 * Property for setting factory for OWLIM based modules and connectors. </p>
	 * 
	 * If not specified, default factories are used.
	 */
	public static final String OWLIM_DRIVER_FACTORY = "cz.cvut.kbss.ontodriver.owlim-factory";

	/**
	 * Property for setting factory for Sesame based modules and connectors.
	 * </p>
	 * 
	 * If not specified, default factories are used.
	 */
	public static final String SESAME_DRIVER_FACTORY = "cz.cvut.kbss.ontodriver.sesame-factory";

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
	 * native), remote storages their own inference settings. </p>
	 * 
	 * {@code Boolean} value expected, default is false.
	 */
	public static final String SESAME_USE_INFERENCE = "cz.cvut.kbss.ontodriver.sesame.use-inference";

	/**
	 * Property for specifying extra URIs which should be added to the module
	 * extraction signature. </p>
	 * 
	 * The module extraction signature is generated from metamodel, but
	 * <i>types</i> and <i>properties</i> cannot be determined from the
	 * metamodel. Therefore it is possible to specify them using this property
	 * so that the module is complete.
	 */
	public static final String MODULE_EXTRACTION_SIGNATURE = "cz.cvut.kbss.ontodriver.module-signature";

	/**
	 * Property representing module extraction signature delimiter. </p>
	 * 
	 * I. e. URIs in module extraction signature are delimited by this string.
	 * 
	 * @see #MODULE_EXTRACTION_SIGNATURE
	 */
	public static final String SIGNATURE_DELIMITER = "|";

	private OntoDriverProperties() {
		throw new AssertionError();
	}

}

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
	 * not affected by this setting.
	 */
	public static final String SESAME_USE_VOLATILE_STORAGE = "cz.cvut.kbss.ontodriver.sesame.use-volatile-storage";

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

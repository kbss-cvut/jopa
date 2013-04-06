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

	private OntoDriverProperties() {
		throw new AssertionError();
	}

}

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

	private OntoDriverProperties() {
		throw new AssertionError();
	}

}

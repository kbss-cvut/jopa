package cz.cvut.kbss.ontodriver.impl.utils;

import cz.cvut.kbss.ontodriver.OntoDriverProperties;

public final class OntoDriverConstants {

	/**
	 * Private constructor.
	 */
	private OntoDriverConstants() {
		throw new AssertionError();
	}

	/**
	 * Default reasoner factory class. </p>
	 * 
	 * The reasoner factory class can be specified by
	 * {@link OntoDriverProperties#OWLAPI_REASONER_FACTORY_CLASS} in properties
	 * passed to the driver.
	 */
	public static final String REASONER_FACTORY_CLASS = "com.clarkparsia.pellet.owlapiv3.PelletReasonerFactory";

	/**
	 * Name of the Turtle ontology format for Jena.
	 */
	public static final String TURTLE_FORMAT = "TURTLE";
}

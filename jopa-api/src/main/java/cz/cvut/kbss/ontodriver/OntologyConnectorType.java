package cz.cvut.kbss.ontodriver;

public enum OntologyConnectorType {
	/**
	 * OWL API ontology connector.
	 */
	OWLAPI(OntoDriverProperties.OWLAPI_DRIVER_FACTORY),
	/**
	 * Jena ontology connector.
	 */
	JENA(OntoDriverProperties.JENA_DRIVER_FACTORY),
	/**
	 * OWLIM ontology connector.
	 */
	OWLIM(OntoDriverProperties.OWLIM_DRIVER_FACTORY),
	/**
	 * Sesame ontology connector.
	 */
	SESAME(OntoDriverProperties.SESAME_DRIVER_FACTORY);

	private final String property;

	private OntologyConnectorType(String property) {
		this.property = property;
	}

	public String getProperty() {
		return property;
	}
}

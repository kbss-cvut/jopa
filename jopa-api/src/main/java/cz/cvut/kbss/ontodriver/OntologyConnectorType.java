package cz.cvut.kbss.ontodriver;

public enum OntologyConnectorType {
	/**
	 * OWL API ontology connector. Name: owlapi
	 */
	OWLAPI("owlapi", OntoDriverProperties.OWLAPI_DRIVER_FACTORY, ""),
	/**
	 * Jena ontology connector. Name: jena
	 */
	JENA("jena", OntoDriverProperties.JENA_DRIVER_FACTORY, ""),
	/**
	 * Sesame ontology connector. Name: sesame
	 */
	SESAME("sesame", OntoDriverProperties.SESAME_DRIVER_FACTORY, "cz.cvut.kbss.ontodriver.sesame.SesameDataSource");

	private final String name;

	private final String property;

	private final String dataSource;

	private OntologyConnectorType(String name, String property, String dataSource) {
		this.name = name;
		this.property = property;
		this.dataSource = dataSource;
	}

	public String getProperty() {
		return property;
	}

	public String getName() {
		return name;
	}

	public String getDataSource() {
		return dataSource;
	}

	/**
	 * Returns value of this enum corresponding to the specified string.
	 * 
	 * @param str
	 *            String to parse
	 * @return {@code OntologyConnectorType} corresponding to the specified
	 *         argument
	 * @throws NullPointerException
	 *             if {@code str} is {@code null}
	 * @throws IllegalArgumentException
	 *             If the argument is cannot be matched to any of the enum
	 *             values
	 */
	public static OntologyConnectorType fromString(String str) {
		if (str == null) {
			throw new NullPointerException();
		}
		for (OntologyConnectorType t : OntologyConnectorType.values()) {
			if (t.name.equalsIgnoreCase(str)) {
				return t;
			}
		}
		throw new IllegalArgumentException(str + " is not a valid "
				+ OntologyConnectorType.class.getName() + " value.");
	}
}

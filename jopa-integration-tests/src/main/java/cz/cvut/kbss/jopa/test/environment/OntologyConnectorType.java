package cz.cvut.kbss.jopa.test.environment;

public enum OntologyConnectorType {
    /**
     * OWL API ontology connector. Name: owlapi
     */
    OWLAPI("owlapi", "cz.cvut.kbss.ontodriver.owlapi.OwlapiDataSource"),
    /**
     * Jena ontology connector. Name: jena
     */
    JENA("jena", ""),
    /**
     * Sesame ontology connector. Name: sesame
     */
    SESAME("sesame", "cz.cvut.kbss.ontodriver.sesame.SesameDataSource");

    private final String name;

    private final String dataSource;

    OntologyConnectorType(String name, String dataSource) {
        this.name = name;
        this.dataSource = dataSource;
    }

    public String getName() {
        return name;
    }

    public String getDriverClass() {
        return dataSource;
    }
}

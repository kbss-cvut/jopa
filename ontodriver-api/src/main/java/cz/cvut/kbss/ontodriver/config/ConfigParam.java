package cz.cvut.kbss.ontodriver.config;

/**
 * Configuration parameters common to all OntoDrivers.
 * <p>
 * Based on {@link OntoDriverProperties}
 */
public enum ConfigParam implements ConfigurationParameter {

    AUTO_COMMIT(OntoDriverProperties.CONNECTION_AUTO_COMMIT),
    REASONER_FACTORY_CLASS(OntoDriverProperties.OWLAPI_REASONER_FACTORY_CLASS),
    ONTOLOGY_LANGUAGE(OntoDriverProperties.ONTOLOGY_LANGUAGE),
    USE_TRANSACTIONAL_ONTOLOGY(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY),
    MODULE_EXTRACTION_SIGNATURE(OntoDriverProperties.MODULE_EXTRACTION_SIGNATURE);

    private final String name;

    ConfigParam(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return name;
    }
}

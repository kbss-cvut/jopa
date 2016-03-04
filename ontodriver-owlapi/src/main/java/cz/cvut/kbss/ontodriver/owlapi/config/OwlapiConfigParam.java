package cz.cvut.kbss.ontodriver.owlapi.config;

import cz.cvut.kbss.ontodriver.config.ConfigurationParameter;

/**
 * Configuration parameters for the OWL API driver.
 */
public enum OwlapiConfigParam implements ConfigurationParameter {

    MAPPING_FILE_LOCATION(OwlapiOntoDriverProperties.MAPPING_FILE_LOCATION),
    IRI_MAPPING_DELIMITER(OwlapiOntoDriverProperties.IRI_MAPPING_DELIMITER),
    WRITE_ON_COMMIT(OwlapiOntoDriverProperties.WRITE_ON_COMMIT);

    private final String name;

    OwlapiConfigParam(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return name;
    }
}

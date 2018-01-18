package cz.cvut.kbss.ontodriver.jena.config;

import cz.cvut.kbss.ontodriver.config.ConfigurationParameter;

public enum JenaConfigParam implements ConfigurationParameter {

    ISOLATION_STRATEGY(JenaOntoDriverProperties.JENA_ISOLATION_STRATEGY),
    STORAGE_TYPE(JenaOntoDriverProperties.JENA_STORAGE_TYPE);

    private final String name;

    JenaConfigParam(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return name;
    }
}

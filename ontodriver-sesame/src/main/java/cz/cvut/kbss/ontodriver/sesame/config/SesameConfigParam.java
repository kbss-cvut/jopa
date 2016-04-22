package cz.cvut.kbss.ontodriver.sesame.config;

import cz.cvut.kbss.ontodriver.config.ConfigurationParameter;

public enum SesameConfigParam implements ConfigurationParameter {

    USE_VOLATILE_STORAGE(SesameOntoDriverProperties.SESAME_USE_VOLATILE_STORAGE),
    USE_INFERENCE(SesameOntoDriverProperties.SESAME_USE_INFERENCE);

    private final String name;

    SesameConfigParam(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return name;
    }
}

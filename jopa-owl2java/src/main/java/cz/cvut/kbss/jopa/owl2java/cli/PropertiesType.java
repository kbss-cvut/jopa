package cz.cvut.kbss.jopa.owl2java.cli;

import cz.cvut.kbss.jopa.owl2java.config.Defaults;

public enum PropertiesType {
    object, string;

    public static PropertiesType fromParam(Object param) {
        return valueOf(param != null ? param.toString() : Defaults.PROPERTIES_TYPE);
    }
}

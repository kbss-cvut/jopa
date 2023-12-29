/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.ontodriver.rdf4j.config;

import cz.cvut.kbss.ontodriver.config.ConfigurationParameter;
import cz.cvut.kbss.ontodriver.config.OntoDriverProperties;

/**
 * RDF4J driver configuration parameters.
 *
 * @see OntoDriverProperties
 * @see Rdf4jOntoDriverProperties
 */
public enum Rdf4jConfigParam implements ConfigurationParameter {

    USE_VOLATILE_STORAGE(Rdf4jOntoDriverProperties.USE_VOLATILE_STORAGE),
    USE_INFERENCE(Rdf4jOntoDriverProperties.USE_INFERENCE),
    LOAD_ALL_THRESHOLD(Rdf4jOntoDriverProperties.LOAD_ALL_THRESHOLD),
    USERNAME(OntoDriverProperties.DATA_SOURCE_USERNAME),
    PASSWORD(OntoDriverProperties.DATA_SOURCE_PASSWORD),
    REPOSITORY_CONFIG(Rdf4jOntoDriverProperties.REPOSITORY_CONFIG),
    RECONNECT_ATTEMPTS(Rdf4jOntoDriverProperties.RECONNECT_ATTEMPTS),
    INFERENCE_IN_DEFAULT_CONTEXT(Rdf4jOntoDriverProperties.INFERENCE_IN_DEFAULT_CONTEXT),
    CONNECTION_REQUEST_TIMEOUT(Rdf4jOntoDriverProperties.CONNECTION_REQUEST_TIMEOUT),
    MAX_CONNECTION_POOL_SIZE(Rdf4jOntoDriverProperties.MAX_CONNECTION_POOL_SIZE);

    private final String name;

    Rdf4jConfigParam(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return name;
    }
}

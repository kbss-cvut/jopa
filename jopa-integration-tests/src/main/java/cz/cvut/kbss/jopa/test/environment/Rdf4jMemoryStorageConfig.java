/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.test.environment;

import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;

import java.util.HashMap;
import java.util.Map;

/**
 * Persistent storage configuration for Sesame in-memory store.
 */
public class Rdf4jMemoryStorageConfig extends StorageConfig {

    private static final OntologyConnectorType TYPE = OntologyConnectorType.RDF4J;

    @Override
    public Map<String, String> createStorageConfiguration() {
        assert name != null;

        final String base = name + TYPE;

        final Map<String, String> config = new HashMap<>();
        config.put(JOPAPersistenceProperties.DATA_SOURCE_CLASS, TYPE.getDriverClass());
        config.put(JOPAPersistenceProperties.ONTOLOGY_URI_KEY, TestEnvironment.IRI_BASE + base);
        config.put(JOPAPersistenceProperties.ONTOLOGY_PHYSICAL_URI_KEY, base);
        return config;
    }
}

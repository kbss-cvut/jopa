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
package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;

import java.util.HashMap;
import java.util.Map;

public class InferenceConnectorFactory extends SharedConnectorBasedConnectorFactory {

    private final Map<String, String> reasonerConfig;

    public InferenceConnectorFactory(DriverConfiguration configuration, Map<String, String> reasonerConfig) {
        super(configuration);
        this.reasonerConfig = new HashMap<>(reasonerConfig);
    }

    @Override
    public StorageConnector createConnector() {
        ensureOpen();
        return new SnapshotStorageConnectorWithInference(centralConnector, reasonerConfig);
    }

    @Override
    public InferredStorageConnector createInferredConnector(StorageConnector connector) {
        assert connector instanceof SnapshotStorageConnectorWithInference;
        return (InferredStorageConnector) connector;
    }
}

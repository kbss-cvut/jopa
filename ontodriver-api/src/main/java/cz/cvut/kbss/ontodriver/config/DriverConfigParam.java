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
package cz.cvut.kbss.ontodriver.config;

/**
 * Configuration parameters common to all OntoDrivers.
 * <p>
 * Based on {@link OntoDriverProperties}
 */
public enum DriverConfigParam implements ConfigurationParameter {

    AUTO_COMMIT(OntoDriverProperties.CONNECTION_AUTO_COMMIT),
    REASONER_FACTORY_CLASS(OntoDriverProperties.REASONER_FACTORY_CLASS),
    USE_TRANSACTIONAL_ONTOLOGY(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY),
    MODULE_EXTRACTION_SIGNATURE(OntoDriverProperties.MODULE_EXTRACTION_SIGNATURE);

    private final String name;

    DriverConfigParam(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return name;
    }
}

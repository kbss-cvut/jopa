/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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

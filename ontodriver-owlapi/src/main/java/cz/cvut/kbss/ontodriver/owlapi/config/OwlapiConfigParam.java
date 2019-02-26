/**
 * Copyright (C) 2019 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
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

/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.sesame.config;

import cz.cvut.kbss.ontodriver.config.ConfigurationParameter;
import cz.cvut.kbss.ontodriver.config.OntoDriverProperties;

/**
 * Sesame driver configuration parameters.
 *
 * @see OntoDriverProperties
 * @see SesameOntoDriverProperties
 */
public enum SesameConfigParam implements ConfigurationParameter {

    USE_VOLATILE_STORAGE(SesameOntoDriverProperties.SESAME_USE_VOLATILE_STORAGE),
    USE_INFERENCE(SesameOntoDriverProperties.SESAME_USE_INFERENCE),
    LOAD_ALL_THRESHOLD(SesameOntoDriverProperties.SESAME_LOAD_ALL_THRESHOLD),
    USERNAME(OntoDriverProperties.DATA_SOURCE_USERNAME),
    PASSWORD(OntoDriverProperties.DATA_SOURCE_PASSWORD),
    REPOSITORY_CONFIG(SesameOntoDriverProperties.SESAME_REPOSITORY_CONFIG),
    RECONNECT_ATTEMPTS(SesameOntoDriverProperties.SESAME_RECONNECT_ATTEMPTS);

    private final String name;

    SesameConfigParam(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return name;
    }
}

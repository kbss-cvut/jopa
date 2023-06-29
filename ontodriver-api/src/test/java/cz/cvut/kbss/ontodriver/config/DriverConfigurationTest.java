/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.config;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class DriverConfigurationTest {

    private final DriverConfiguration configuration = new DriverConfiguration(
            OntologyStorageProperties.driver("cz.cvut.kbss.ontodriver.sesame.SesameDataSource")
                                     .physicalUri("memory-store").build());

    @Test
    void addConfigurationFiltersKnownConfigurationProperties() {
        final Map<String, String> props = new HashMap<>();
        props.put(OntoDriverProperties.CONNECTION_AUTO_COMMIT, "true");
        props.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, "false");

        configuration.addConfiguration(props,
                Arrays.asList(DriverConfigParam.MODULE_EXTRACTION_SIGNATURE,
                        DriverConfigParam.USE_TRANSACTIONAL_ONTOLOGY));
        assertNull(configuration.getProperty(DriverConfigParam.AUTO_COMMIT)); // Not in the known properties
        assertNotNull(configuration.getProperty(DriverConfigParam.USE_TRANSACTIONAL_ONTOLOGY));
    }

    @Test
    void getWithDefaultReturnsDefaultWhenConfigParamValueIsNotFound() {
        assertNull(configuration.getProperty(DriverConfigParam.USE_TRANSACTIONAL_ONTOLOGY));
        final String defaultValue = "false";
        assertEquals(defaultValue,
                configuration.getProperty(DriverConfigParam.USE_TRANSACTIONAL_ONTOLOGY, defaultValue));
    }

    @Test
    void testGetPropertyAsBoolean() {
        configuration.setProperty(DriverConfigParam.AUTO_COMMIT, Boolean.TRUE.toString());
        assertTrue(configuration.is(DriverConfigParam.AUTO_COMMIT));
    }

    @Test
    void getAsBooleanReturnsFalseForUnknownProperty() {
        assertFalse(configuration.is(DriverConfigParam.AUTO_COMMIT));
    }
}

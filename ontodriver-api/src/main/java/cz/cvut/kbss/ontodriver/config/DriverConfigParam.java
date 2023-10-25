/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
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
    /**
     * @deprecated Ontology language should be set at persistence unit level. The driver uses language specified per
     * {@link cz.cvut.kbss.ontodriver.model.Assertion}.
     */
    @Deprecated
    ONTOLOGY_LANGUAGE(OntoDriverProperties.ONTOLOGY_LANGUAGE),
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

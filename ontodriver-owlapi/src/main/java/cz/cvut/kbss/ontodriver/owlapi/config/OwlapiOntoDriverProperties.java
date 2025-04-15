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
package cz.cvut.kbss.ontodriver.owlapi.config;

public abstract class OwlapiOntoDriverProperties {

    /**
     * Represents location of a OWLAPI mapping file.
     * <p>
     * This file is used to map logical IRIs of ontologies to their physical locations, e.g. in case they are not
     * accessible online, but are stored in files locally.
     */
    public static final String MAPPING_FILE_LOCATION = "cz.cvut.kbss.ontodriver.owlapi.mappingFile";

    /**
     * Delimits IRI mappings in the mapping file.
     *
     * @see #MAPPING_FILE_LOCATION
     */
    public static final String IRI_MAPPING_DELIMITER = "cz.cvut.kbss.ontodriver.owlapi.mapping-delimiter";

    /**
     * When set to true, the driver will write the content of the ontology into its file after commit.
     */
    public static final String WRITE_ON_COMMIT = "cz.cvut.kbss.ontodriver.owlapi.writeOnCommit";

    /**
     * Default IRI mapping delimiter.
     *
     * @see #IRI_MAPPING_DELIMITER
     */
    public static final String DEFAULT_IRI_MAPPING_DELIMITER = ">";

    private OwlapiOntoDriverProperties() {
        throw new AssertionError();
    }
}

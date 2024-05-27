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
package cz.cvut.kbss.ontodriver.config;

public final class OntoDriverProperties {

    /**
     * Property for setting default auto-commit strategy for connections.
     */
    public static final String CONNECTION_AUTO_COMMIT = "cz.cvut.kbss.ontodriver.connection-auto-commit";

    /**
     * Username to be used when accessing data source.
     */
    public static final String DATA_SOURCE_USERNAME = "cz.cvut.jopa.dataSource.username";

    /**
     * Password to be used when accessing data source.
     */
    public static final String DATA_SOURCE_PASSWORD = "cz.cvut.jopa.dataSource.password";

    /**
     * Reasoner factory class property.
     * <p>
     * Applies to OntoDriver implementations which support selection of reasoner factory class, which are currently OWL
     * API and Jena.
     */
    public static final String REASONER_FACTORY_CLASS = "cz.cvut.jopa.reasonerFactoryClass";

    /**
     * This setting tells the driver whether to use the transactional ontology for retrieving entities and answering
     * queries.
     * <p>
     * If so, uncommitted changes made during transaction will be included in query evaluation, entity retrieval etc.
     * Otherwise the driver will use the ontology as it was when the transaction was started and uncommitted changes
     * will not be visible until commit.
     */
    public static final String USE_TRANSACTIONAL_ONTOLOGY = "cz.cvut.kbss.ontodriver.use-transactional-onto";

    /**
     * Property for specifying extra URIs which should be added to the module extraction signature.
     * <p>
     * The module extraction signature is generated from metamodel, but
     * <i>types</i> and <i>properties</i> cannot be determined from the
     * metamodel. Therefore it is possible to specify them using this property so that the module is complete.
     */
    public static final String MODULE_EXTRACTION_SIGNATURE = "cz.cvut.kbss.ontodriver.module-signature";

    /**
     * Property representing module extraction signature delimiter.
     * <p>
     * I. e. URIs in module extraction signature are delimited by this string.
     *
     * @see #MODULE_EXTRACTION_SIGNATURE
     */
    public static final String SIGNATURE_DELIMITER = "|";

    private OntoDriverProperties() {
        throw new AssertionError();
    }
}

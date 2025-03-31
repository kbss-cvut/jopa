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
package cz.cvut.kbss.ontodriver.rdf4j.connector.init;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.connector.ConnectionFactory;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.loader.StatementLoaderFactory;

/**
 * Builds factories for the driver.
 */
public interface FactoryOfFactories {

    /**
     * Creates a {@link ConnectionFactory} instance.
     *
     * @return New connection factory
     * @throws Rdf4jDriverException When unable to init the factory (for example, unable to connect to the repository)
     */
    ConnectionFactory createConnectorFactory() throws OntoDriverException;

    /**
     * Creates a {@link StatementLoaderFactory} instance.
     *
     * @return New statement loader factory
     */
    StatementLoaderFactory createStatementLoaderFactory();
}

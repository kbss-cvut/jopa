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
package cz.cvut.kbss.ontodriver.rdf4j.connector.init;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.rdf4j.config.Rdf4jConfigParam;
import cz.cvut.kbss.ontodriver.rdf4j.connector.ConnectionFactoryConfig;
import cz.cvut.kbss.ontodriver.rdf4j.environment.TestUtils;
import org.eclipse.rdf4j.common.transaction.IsolationLevels;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

class FactoryOfFactoriesTest {

    @Test
    void initializationResolvesThatUnderlyingRepoIsNotGraphDBWhenItDoesNotContainGraphDBInternalIdStatements() throws Exception {
        final DriverConfiguration conf = TestUtils.createDriverConfig("test");
        conf.setProperty(Rdf4jConfigParam.USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
        final Rdf4jFactoryOfFactories sut = new Rdf4jFactoryOfFactories(conf);
        assertFalse(sut.resolveFactoryConfig().isGraphDB());
    }

    @Test
    void resolveFactoryConfigResolvesTransactionIsolationLevelFromConfiguration() throws Exception {
        final DriverConfiguration conf = TestUtils.createDriverConfig("test");
        conf.setProperty(Rdf4jConfigParam.USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
        conf.setProperty(Rdf4jConfigParam.TRANSACTION_ISOLATION_LEVEL, IsolationLevels.SERIALIZABLE.toString());
        final Rdf4jFactoryOfFactories sut = new Rdf4jFactoryOfFactories(conf);
        final ConnectionFactoryConfig result = sut.resolveFactoryConfig();
        assertEquals(IsolationLevels.SERIALIZABLE, result.txIsolationLevel());
    }
}

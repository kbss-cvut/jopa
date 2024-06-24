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
package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.jena.config.JenaOntoDriverProperties;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;

public class JenaDataSourceTest {

    private OntologyStorageProperties storageProps;
    private final Map<String, String> properties = new HashMap<>();

    private final JenaDataSource dataSource = new JenaDataSource();

    @BeforeEach
    public void setUp() {
        this.storageProps = OntologyStorageProperties.driver(JenaDataSource.class.getName())
                                                     .physicalUri(URI.create("temp:memory")).build();
        properties.put(JenaOntoDriverProperties.JENA_STORAGE_TYPE, JenaOntoDriverProperties.IN_MEMORY);
        properties.put(JenaOntoDriverProperties.JENA_ISOLATION_STRATEGY, JenaOntoDriverProperties.READ_COMMITTED);
    }

    @Test
    public void getConnectionAcquiresConnectionToUnderlyingStorage() {
        dataSource.setStorageProperties(storageProps);
        dataSource.setProperties(properties);
        final Connection connection = dataSource.getConnection();
        assertNotNull(connection);
        assertTrue(dataSource.isOpen());
        assertTrue(connection.isOpen());
    }

    @Test
    public void getConnectionThrowsIllegalStateWhenDataSourceIsClosed() throws Exception {
        dataSource.close();
        assertThrows(IllegalStateException.class, dataSource::getConnection);
    }

    @Test
    public void closeClosesUnderlyingDriver() throws Exception {
        dataSource.setStorageProperties(storageProps);
        dataSource.setProperties(properties);
        final Connection connection = dataSource.getConnection();
        dataSource.close();
        // Closing driver closes connections
        assertFalse(connection.isOpen());
    }

    @Test
    public void connectingWithoutStoragePropertiesThrowsIllegalStateException() {
        final IllegalStateException ex = assertThrows(IllegalStateException.class, dataSource::getConnection);
        assertThat(ex.getMessage(), containsString("cannot connect without ontology storage properties"));
    }
}

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
package cz.cvut.kbss.jopa.test.integration.virtuoso.environment;

import cz.cvut.kbss.jopa.Persistence;
import cz.cvut.kbss.jopa.model.EntityManagerFactory;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProvider;

import java.util.HashMap;
import java.util.Map;

public class PersistenceFactory {

    public static EntityManagerFactory initPersistence(Map<String, String> properties) {
        final Map<String, String> props = new HashMap<>();
        props.put(JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa.test");
        props.put(JOPAPersistenceProperties.DATA_SOURCE_CLASS, TestDataSource.class.getCanonicalName());
        props.put(JOPAPersistenceProperties.JPA_PERSISTENCE_PROVIDER, JOPAPersistenceProvider.class.getName());
        props.put(JOPAPersistenceProperties.ONTOLOGY_PHYSICAL_URI_KEY, "TestOntology");
        props.putAll(properties);
        return Persistence.createEntityManagerFactory("testPU", props);
    }
}

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
package cz.cvut.kbss.jopa.test.environment;

import cz.cvut.kbss.jopa.Persistence;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProvider;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

public class TestEnvironment {

    public static final String PERSISTENCE_LANGUAGE = "en";

    public static final String TEST_RESULTS_DIR = "testResults";

    public static final String REASONER_FACTORY_CLASS = "com.clarkparsia.pellet.owlapiv3.PelletReasonerFactory";
    public static final String IRI_BASE = "http://krizik.felk.cvut.cz/ontologies/2013/jopa-tests/";

    public static final String EXPLICIT_DATATYPE = "http://www.w3.org/ns/csvw#uriTemplate";

    /**
     * True if the ontology file should be deleted before access to it is initialized. This effectively means that the
     * test will create the ontology from scratch. Default value is true.
     */
    public static final boolean DELETE_ONTOLOGY_FILE = true;

    private TestEnvironment() {
        throw new AssertionError();
    }

    public static EntityManager getPersistenceConnector(String name, StorageConfig storage,
                                                        boolean cache, Map<String, String> properties) {
        final Map<String, String> params = initParams(cache);
        // Can override default params
        params.putAll(properties);
        storage.setName(name);
        storage.setDirectory(TEST_RESULTS_DIR);
        final Map<String, String> config = new HashMap<>(storage.createStorageConfiguration());
        config.putAll(params);
        return Persistence.createEntityManagerFactory("Test_" + name, config)
                                            .createEntityManager();
    }

    private static Map<String, String> initParams(boolean cache) {
        final Map<String, String> params = new HashMap<>();
        if (cache) {
            params.put(JOPAPersistenceProperties.CACHE_ENABLED, "true");
        } else {
            params.put(JOPAPersistenceProperties.CACHE_ENABLED, "false");
        }
        /* Set location of the entities (package) */
        params.put(JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa.test");
        params.put(JOPAPersistenceProperties.JPA_PERSISTENCE_PROVIDER, JOPAPersistenceProvider.class.getName());
        return params;
    }

    /**
     * Removes (recursively) the specified file/directory.
     * <p>
     * The removal is executed only if the file exists and {@code deleteOntologyFile} is set to {@code true}.
     *
     * @param file The file/directory to remove
     */
    public static void removeOldTestFiles(File file) {
        if (file.exists() && DELETE_ONTOLOGY_FILE) {
            if (file.isDirectory() && file.listFiles() != null) {
                for (File c : file.listFiles()) {
                    removeOldTestFiles(c);
                }
                file.delete();
            } else {
                if (!file.delete()) {
                    throw new IllegalStateException("Unable to delete file " + file);
                }
            }
        }
    }
}

/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.test.environment;

import cz.cvut.kbss.jopa.Persistence;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProvider;

import java.io.File;
import java.util.*;

public class TestEnvironment {

    public static final String dir = "testResults";

    public static final String REASONER_FACTORY_CLASS = "com.clarkparsia.pellet.owlapiv3.PelletReasonerFactory";
    public static final String IRI_BASE = "http://krizik.felk.cvut.cz/ontologies/2013/jopa-tests/";

    /**
     * True if the ontology file should be deleted before access to it is initialized. This effectively means that the
     * test will create the ontology from scratch. Default value is true.
     */
    public static final boolean DELETE_ONTOLOGY_FILE = true;

    // private static final String REASONER_FACTORY_CLASS =
    // "org.semanticweb.HermiT.Reasoner$ReasonerFactory";

    /**
     * Creates persistence connector, with enabled second level cache, for OWLAPI accessed ontology stored in a file.
     *
     * @param name Base name used for ontology URI and physical storage path/URI
     * @return Persistence context
     */
    public static EntityManager getPersistenceConnector(String name) {
        return getOwlapiPersistenceConnector(name, true);
    }

    /**
     * Creates persistence connector for OWLAPI accessed ontology stored in a file.
     *
     * @param name  Base name used for ontology URI and physical storage path/URI
     * @param cache Whether second level cache should be enabled
     * @return Persistence context
     */
    public static EntityManager getPersistenceConnector(String name, boolean cache) {
        return getOwlapiPersistenceConnector(name, cache);
    }

    private static EntityManager getOwlapiPersistenceConnector(String name, boolean cache) {
        return getPersistenceConnector(name,
                Collections.<StorageConfig>singletonList(new OwlapiStorageConfig()), cache).get(0);
    }

    public static EntityManager getPersistenceConnector(String name, StorageConfig storage,
                                                        boolean cache, Map<String, String> properties) {
        return getPersistenceConnector(name, Collections.singletonList(storage), cache, properties)
                .get(0);
    }

    /**
     * Creates persistence connector for the specified list of storages.
     *
     * @param baseName Base name used for ontology URI and physical storage path/URI
     * @param storages List of storage configurations
     * @param cache    Whether second level cache should be enabled
     * @return Persistence context
     */
    public static List<EntityManager> getPersistenceConnector(String baseName,
                                                              List<StorageConfig> storages, boolean cache) {
        return getPersistenceConnector(baseName, storages, cache,
                Collections.<String, String>emptyMap());
    }

    /**
     * Creates persistence connector for the specified list of storages.
     *
     * @param baseName Base name used for ontology URI and physical storage path/URI
     * @param storages List of storage configurations
     * @param cache    Whether second level cache should be enabled
     * @param props    Additional properties for the persistence provider
     * @return Persistence context
     */
    public static List<EntityManager> getPersistenceConnector(String baseName,
                                                              List<StorageConfig> storages, boolean cache,
                                                              Map<String, String> props) {
        final Map<String, String> params = initParams(cache);
        // Can override default params
        params.putAll(props);
        int i = 1;
        final List<EntityManager> managers = new ArrayList<>(storages.size());
        for (StorageConfig si : storages) {
            si.setName(baseName);
            si.setDirectory(dir);
            final Map<String, String> config = si.createStorageConfiguration(i++);
            config.putAll(params);
            final EntityManager em = Persistence.createEntityManagerFactory("context-name_" + i, config)
                                                .createEntityManager();
            managers.add(em);
        }
        return managers;
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
        params.put(JOPAPersistenceProperties.JPA_PERSISTENCE_PROVIDER,
                JOPAPersistenceProvider.class.getName());
        params.put(JOPAPersistenceProperties.REASONER_FACTORY_CLASS, REASONER_FACTORY_CLASS);
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
                for (File c : file.listFiles())
                    removeOldTestFiles(c);
                assert file.delete();
            } else {
                if (!file.delete()) {
                    throw new RuntimeException("Unable to delete file " + file);
                }
            }
        }
    }
}

/**
 * Copyright (C) 2011 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.example02;

import com.clarkparsia.pellet.owlapiv3.PelletReasonerFactory;
import cz.cvut.kbss.jopa.Persistence;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.EntityManagerFactory;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProvider;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.Map;

public class PersistenceFactory {

    private static final Logger LOG = LoggerFactory.getLogger(PersistenceFactory.class);

    private static final String REPOSITORY_FILE_NAME = "repository.owl";
    private static final String FILE_SCHEMA = "file://";

    private static boolean initialized = false;

    private static EntityManagerFactory emf;

    private PersistenceFactory() {
        throw new AssertionError();
    }

    public static void init(String ontologyFile) {
        final Map<String, String> props = new HashMap<>();
        // Here we set up basic storage access properties - driver class, physical location of the storage
        props.put(JOPAPersistenceProperties.ONTOLOGY_PHYSICAL_URI_KEY, setupRepository(ontologyFile));
        props.put(JOPAPersistenceProperties.ONTOLOGY_URI_KEY, "http://krizik.felk.cvut.cz/ontologies/jopa/example02");
        props.put(JOPAPersistenceProperties.DATA_SOURCE_CLASS, "cz.cvut.kbss.ontodriver.owlapi.OwlapiDataSource");
        // View transactional changes during transaction
        props.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
        // Ontology language
        props.put(JOPAPersistenceProperties.LANG, "en");
        // Where to look for entity classes
        props.put(JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa.example02.model");
        // Use Pellet for reasoning
        props.put(OntoDriverProperties.OWLAPI_REASONER_FACTORY_CLASS, PelletReasonerFactory.class.getName());
        // Persistence provider name
        props.put(JOPAPersistenceProperties.JPA_PERSISTENCE_PROVIDER, JOPAPersistenceProvider.class.getName());

        emf = Persistence.createEntityManagerFactory("jopaExample02PU", props);
        initialized = true;
    }

    private static String setupRepository(String ontologyFile) {
        LOG.debug("Setting up repository...");
        final String ontologyFileAbsolute = resolveAbsolutePath(ontologyFile);
        final String repoFolder = ontologyFileAbsolute.substring(0,
                ontologyFileAbsolute.lastIndexOf(File.separatorChar));
        final File repoFile = new File(repoFolder + File.separator + REPOSITORY_FILE_NAME);
        if (repoFile.exists()) {
            LOG.debug("Repository already exists. Removing it...");
            final boolean res = repoFile.delete();
            assert res;
        }
        try {
            LOG.debug("Copying ontology to the repository...");
            Files.copy(new File(ontologyFileAbsolute).toPath(), repoFile.toPath());
        } catch (IOException e) {
            LOG.error("Unable to copy ontology into the repository", e);
            System.exit(1);
        }
        return URI.create(FILE_SCHEMA + repoFile.getAbsolutePath()).toString();
    }

    private static String resolveAbsolutePath(String ontologyFile) {
        final File file = new File(ontologyFile);
        assert file.exists();
        return file.getAbsolutePath();
    }

    public static EntityManager createEntityManager() {
        if (!initialized) {
            throw new IllegalStateException("PersistenceFactory has not been initialized.");
        }
        return emf.createEntityManager();
    }

    public static void close() {
        emf.close();
    }
}

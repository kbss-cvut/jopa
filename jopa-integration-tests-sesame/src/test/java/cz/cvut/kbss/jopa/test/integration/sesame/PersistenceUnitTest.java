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
package cz.cvut.kbss.jopa.test.integration.sesame;

import cz.cvut.kbss.jopa.Persistence;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.EntityManagerFactory;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProvider;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.SesameDataAccessor;
import cz.cvut.kbss.jopa.test.environment.SesamePersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.PersistenceUnitTestRunner;
import cz.cvut.kbss.ontodriver.config.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.sesame.SesameDataSource;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

import static org.junit.Assert.assertNotNull;

public class PersistenceUnitTest extends PersistenceUnitTestRunner {

    private static final Logger LOG = LoggerFactory.getLogger(PersistenceUnitTest.class);

    public PersistenceUnitTest() {
        super(LOG, new SesamePersistenceFactory(), new SesameDataAccessor());
    }

    @Test
    public void multiplePersistenceUnitsOnLocalNativeStoreCanExistSimultaneously() throws Exception {
        final File dir = Files.createTempDirectory("sesame-native-test").toFile();
        try {
            final int count = Generators.randomPositiveInt(2, 5);
            final List<EntityManagerFactory> emfs = new ArrayList<>();
            for (int i = 0; i < count; i++) {
                emfs.add(buildLocalNativeEmf("testPu" + i, dir));
            }
            final List<OWLClassA> instances = generateTestData(emfs);
            verifyTestData(emfs, instances);
            emfs.forEach(EntityManagerFactory::close);
        } finally {
            recursivelyDeleteDirectory(dir);
        }
    }

    private EntityManagerFactory buildLocalNativeEmf(String puName, File directory) {
        final Map<String, String> config = new HashMap<>();
        config.put(JOPAPersistenceProperties.ONTOLOGY_PHYSICAL_URI_KEY,
                directory.getAbsolutePath() + File.separator + "repositories" + File.separator + "test");
        config.put(JOPAPersistenceProperties.JPA_PERSISTENCE_PROVIDER, JOPAPersistenceProvider.class.getName());
        config.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
        config.put(JOPAPersistenceProperties.LANG, "en");
        config.put(JOPAPersistenceProperties.DATA_SOURCE_CLASS, SesameDataSource.class.getName());
        config.put(JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa.test");
        return Persistence.createEntityManagerFactory(puName, config);
    }

    private List<OWLClassA> generateTestData(List<EntityManagerFactory> emfs) {
        final List<OWLClassA> instances = new ArrayList<>();
        emfs.forEach(emf -> {
            final EntityManager em = emf.createEntityManager();
            try {
                em.getTransaction().begin();
                final OWLClassA a = new OWLClassA();
                a.setUri(Generators.generateUri());
                em.persist(a);
                em.getTransaction().commit();
                instances.add(a);
            } finally {
                em.close();
            }
        });
        return instances;
    }

    private void verifyTestData(List<EntityManagerFactory> emfs, List<OWLClassA> instances) {
        emfs.forEach(emf -> {
            final OWLClassA a = instances.get(0);
            final EntityManager em = emf.createEntityManager();
            try {
                assertNotNull(em.find(OWLClassA.class, a.getUri()));
            } finally {
                em.close();
            }
        });
    }

    private void recursivelyDeleteDirectory(File directory) throws IOException {
        Files.walk(directory.toPath())
             .sorted(Comparator.reverseOrder())
             .map(Path::toFile)
             .forEach(File::delete);
    }
}

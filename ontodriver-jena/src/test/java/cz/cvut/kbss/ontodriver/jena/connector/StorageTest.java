/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverInitializationException;
import cz.cvut.kbss.ontodriver.jena.config.JenaConfigParam;
import cz.cvut.kbss.ontodriver.jena.config.JenaOntoDriverProperties;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import org.apache.jena.query.DatasetFactory;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.vocabulary.RDF;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.nio.file.Files;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;

public class StorageTest extends StorageTestUtil {

    @Test
    public void createInitializesMemoryStorageForMemoryConfiguration() {
        final DriverConfiguration config = createConfiguration("test:uri");
        config.setProperty(JenaConfigParam.STORAGE_TYPE, JenaOntoDriverProperties.IN_MEMORY);
        final Storage result = Storage.create(config);
        assertNotNull(result);
        assertTrue(result instanceof MemoryStorage);
        assertNotNull(result.getDataset());
    }

    @Test
    public void createInitializesFileStorageForFileConfiguration() throws Exception {
        final File file = Files.createTempFile("jena-onto", ".ttl").toFile();
        file.deleteOnExit();
        final DriverConfiguration config = createConfiguration(file.getAbsolutePath());
        config.setProperty(JenaConfigParam.STORAGE_TYPE, JenaOntoDriverProperties.FILE);
        final Storage result = Storage.create(config);
        assertNotNull(result);
        assertTrue(result instanceof FileStorage);
    }

    @Test
    public void createThrowsInitializationExceptionForUnknownStorageType() {
        final DriverConfiguration config = createConfiguration("test:uri");
        config.setProperty(JenaConfigParam.STORAGE_TYPE, "dontKnowThisOne");
        final OntoDriverInitializationException ex = assertThrows(OntoDriverInitializationException.class,
                () -> Storage.create(config));
        assertThat(ex.getMessage(), containsString("Unsupported storage type"));
    }

    @Test
    public void createCreatesInMemoryByDefault() {
        final DriverConfiguration config = createConfiguration("test:uri");
        final Storage result = Storage.create(config);
        assertNotNull(result);
        assertTrue(result instanceof MemoryStorage);
    }

    @Test
    public void getDefaultGraphReturnsUnionModelWhenConfiguredTo() {
        final DriverConfiguration config = createConfiguration("test:uri");
        config.setProperty(JenaConfigParam.TREAT_DEFAULT_GRAPH_AS_UNION, Boolean.toString(true));
        final Storage result = Storage.create(config);
        final String ctx = "test";
        result.getDataset().getNamedModel(ctx).add(ResourceFactory
                .createStatement(ResourceFactory.createResource(), RDF.type, ResourceFactory.createResource(
                        Generator.generateUri().toString())));
        assertFalse(result.getNamedGraph(ctx).isEmpty());
        assertFalse(result.getDefaultGraph().isEmpty());
    }

    @Test
    public void setDatasetThrowsUnsupportedOperationException() throws Exception {
        final File file = Files.createTempFile("jena-onto", ".ttl").toFile();
        file.deleteOnExit();
        final DriverConfiguration config = createConfiguration(file.getAbsolutePath());
        config.setProperty(JenaConfigParam.STORAGE_TYPE, JenaOntoDriverProperties.FILE);
        final Storage storage = Storage.create(config);
        assertNotNull(storage);
        assertThrows(UnsupportedOperationException.class, () -> storage.setDataset(DatasetFactory.create()));
    }
}

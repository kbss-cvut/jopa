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
package cz.cvut.kbss.ontodriver.jena.connector;

import org.apache.jena.query.Dataset;
import org.apache.jena.query.ReadWrite;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.vocabulary.RDF;
import org.junit.jupiter.api.Test;

import java.io.*;
import java.nio.file.Files;
import java.util.List;

import static org.apache.jena.rdf.model.ResourceFactory.createResource;
import static org.apache.jena.rdf.model.ResourceFactory.createStatement;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;

public class FileStorageTest extends StorageTestUtil {

    @Test
    public void initializesStorageByReadingFileWithSingleGraph() throws Exception {
        final File file = Files.createTempFile("jena-onto", ".ttl").toFile();
        file.deleteOnExit();
        final Model model = generateModel();
        try (final BufferedWriter writer = new BufferedWriter(new FileWriter(file))) {
            model.write(writer, "TURTLE");
        }

        final LocalStorage result = new FileStorage(createConfiguration(file.getAbsolutePath()));
        final Dataset dataset = result.getDataset();
        assertNotNull(dataset);
        assertFalse(dataset.getDefaultModel().isEmpty());
    }

    private Model generateModel() {
        final Model model = ModelFactory.createDefaultModel();
        final Resource resource = model.createResource("test:example");
        resource.addProperty(RDF.type, model.createResource("test:type"));
        return model;
    }

    @Test
    public void initializesStorageFromNonexistentFileByCreatingIt() {
        final String filePath = System.getProperty("java.io.tmpdir") + File.separator + "jena-test.ttl";
        final LocalStorage result = new FileStorage(createConfiguration(filePath));
        final Dataset dataset = result.getDataset();
        assertNotNull(dataset);

        final File f = new File(filePath);
        assertTrue(f.exists());
        f.deleteOnExit();
    }

    @Test
    public void writeChangesOutputsChangesIntoTargetFile() throws Exception {
        final File file = Files.createTempFile("jena-onto", ".ttl").toFile();
        file.deleteOnExit();
        final LocalStorage storage = new FileStorage(createConfiguration(file.getAbsolutePath()));
        final Dataset dataset = storage.getDataset();
        final Model m = dataset.getDefaultModel();
        dataset.begin(ReadWrite.WRITE);
        final String rName = "test:example";
        final String rValue = "test:type";
        final Resource resource = m.createResource(rName);
        resource.addProperty(RDF.type, m.createResource(rValue));
        dataset.commit();
        storage.writeChanges();

        final List<String> lines = Files.readAllLines(file.toPath());
        assertFalse(lines.isEmpty());
        final String str = String.join("\n", lines);
        assertThat(str, containsString(rName));
        assertThat(str, containsString(rValue));
    }

    @Test
    public void reloadReloadsDatasetFromFile() throws Exception {
        final File file = Files.createTempFile("jena-onto", ".ttl").toFile();
        file.deleteOnExit();
        final LocalStorage storage = new FileStorage(createConfiguration(file.getAbsolutePath()));
        final Resource subj = createResource(SUBJECT);
        final Resource obj = createResource(TYPE_TWO);
        assertFalse(storage.getDefaultGraph().contains(subj, RDF.type, obj));

        final Model m = RDFDataMgr.loadModel(file.getAbsolutePath());
        m.add(createStatement(subj, RDF.type, obj));
        try (final BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream(file))) {
            RDFDataMgr.write(out, m, Lang.TTL);
        }
        m.close();

        storage.reload();
        assertTrue(storage.getDefaultGraph().contains(subj, RDF.type, obj));
    }

    @Test
    public void reloadThrowsIllegalStateWhenStorageIsInTransaction() throws Exception {
        final File file = Files.createTempFile("jena-onto", ".ttl").toFile();
        file.deleteOnExit();
        final LocalStorage storage = new FileStorage(createConfiguration(file.getAbsolutePath()));

        storage.begin(ReadWrite.WRITE);
        final IllegalStateException ex = assertThrows(IllegalStateException.class, storage::reload);
        assertThat(ex.getMessage(), containsString("Cannot reload storage which is in transaction"));
    }
}

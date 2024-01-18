/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.owl2java;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAxiom;

import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Comparator;
import java.util.Random;

public class TestUtils {

    static final String MAPPING_FILE_NAME = "mapping";
    static final String IC_ONTOLOGY_IRI = "http://krizik.felk.cvut.cz/ontologies/owl2java-ics.owl";
    static final String BAD_IMPORT_ONTOLOGY_IRI = "http://krizik.felk.cvut.cz/ontologies/onto-with-missing-import.owl";
    static final String CONTEXT = "owl2java-ic";

    static final String VOCABULARY_FILE = Constants.VOCABULARY_CLASS + ".java";

    private static final String IRI_BASE = "http://onto.fel.cvut.cz/ontologies/Entity";
    private static final Random RANDOM = new Random();

    private TestUtils() {
        throw new AssertionError();
    }

    public static String resolveTestResourcesFilePath(String fileName) throws UnsupportedEncodingException {
        final URL resource = TestUtils.class.getClassLoader().getResource(fileName);
        final String decodedPath = URLDecoder.decode(resource.getFile(), StandardCharsets.UTF_8.toString());
        final File mf = new File(decodedPath);
        return mf.getAbsolutePath();
    }

    static File getTempDirectory() throws IOException {
        final File targetDir = Files.createTempDirectory("owl2java-test").toFile();
        targetDir.deleteOnExit();
        return targetDir;
    }

    static void recursivelyDeleteDirectory(File directory) throws IOException {
        Files.walk(directory.toPath())
             .sorted(Comparator.reverseOrder())
             .forEach(path -> {
                 try {
                     Files.delete(path);
                 } catch (IOException e) {
                     throw new RuntimeException("Unable to delete file " + path, e);
                 }
             });
    }

    static void addAxiom(OWLAxiom axiom, OWL2JavaTransformer transformer) throws Exception {
        final Field defaultContextField = OWL2JavaTransformer.class.getDeclaredField("defaultContext");
        defaultContextField.setAccessible(true);
        final ContextDefinition defaultContext = (ContextDefinition) defaultContextField.get(transformer);
        final Method getContextMethod = OWL2JavaTransformer.class
                .getDeclaredMethod("getContextDefinition", String.class);
        getContextMethod.setAccessible(true);
        final ContextDefinition testContext = (ContextDefinition) getContextMethod.invoke(transformer, CONTEXT);
        defaultContext.addAxiom(axiom);
        defaultContext.parse();
        testContext.parse();
    }

    static IRI generateIri() {
        return IRI.create(IRI_BASE + RANDOM.nextInt());
    }
}

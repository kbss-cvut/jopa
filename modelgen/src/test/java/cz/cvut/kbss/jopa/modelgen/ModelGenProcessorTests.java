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
package cz.cvut.kbss.jopa.modelgen;

import com.google.testing.compile.Compilation;
import com.google.testing.compile.JavaFileObjects;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

import static com.google.testing.compile.CompilationSubject.assertThat;
import static com.google.testing.compile.Compiler.javac;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ModelGenProcessorTests {

    private static final String OUTPUT_DIRECTORY = "target/generated-test-sources/static-metamodel";

    @AfterEach
    void deleteSMClass() throws IOException {
        deleteTestFile(OUTPUT_DIRECTORY + "/cz/test/ex/TestingClassOWL_.java");
    }

    static String readFileAsString(File file) throws IOException {
        return String.join("\n", Files.readAllLines(file.toPath(), StandardCharsets.UTF_8));
    }

    static void deleteTestFile(String path) throws IOException {
        File file = new File(path);
        Files.deleteIfExists(file.toPath());
    }

    @Test
    void metamodelGenerationProducesCompilableStaticMetamodelClasses() throws Exception {
        List<String> options = List.of("-AoutputDirectory=" + OUTPUT_DIRECTORY, "-AdebugOption=true");
        Compilation compilation = javac()
                .withProcessors(new ModelGenProcessor()).withOptions(options)
                .compile(JavaFileObjects.forSourceLines("cz.test.ex.TestingClassOWL", readFileAsString(
                                new File("src/test/java/cz/test/ex/TestingClassOWL.java"))),
                        JavaFileObjects.forSourceLines("cz.test.ex.TestingClassNonEntity", readFileAsString(
                                new File("src/test/java/cz/test/ex/TestingClassNonEntity.java"))),
                        JavaFileObjects.forSourceLines("cz.test.ex.TestingClassNotOWL", readFileAsString(
                                new File("src/test/java/cz/test/ex/TestingClassNotOWL.java"))));
        assertThat(compilation).succeededWithoutWarnings();

        assertFalse(Files.exists(Paths.get(OUTPUT_DIRECTORY + "/cz/test/ex/TestingClassNonEntity_.java")));
        assertFalse(Files.exists(Paths.get(OUTPUT_DIRECTORY + "/cz/test/ex/TestingClassNotOWL_.java")));

        final Compilation staticMetamodelCompilation = javac()
                .compile(JavaFileObjects.forSourceLines("cz.test.ex.TestingClassOWL_",
                        readFileAsString(new File(OUTPUT_DIRECTORY + "/cz/test/ex/TestingClassOWL_.java"))));

        assertThat(staticMetamodelCompilation).succeededWithoutWarnings();
        assertEquals(readFileAsString(new File("src/test/resources/TestingClassOWL_.txt")),
                readFileAsString(new File(OUTPUT_DIRECTORY + "/cz/test/ex/TestingClassOWL_.java")));
    }

    @Test
    void metamodelGenerationHandlesReferenceToConstantInEntityClass() throws Exception {
        List<String> options = List.of("-AoutputDirectory=" + OUTPUT_DIRECTORY, "-AdebugOption=" + "true");
        Compilation compilation = javac()
                .withProcessors(new ModelGenProcessor()).withOptions(options)
                .compile(JavaFileObjects.forSourceLines("cz.test.ex.TestingClassWithContext", readFileAsString(
                        new File("src/test/java/cz/test/ex/TestingClassWithContext.java"))));
        assertThat(compilation).succeededWithoutWarnings();

        assertTrue(Files.exists(Paths.get(OUTPUT_DIRECTORY + "/cz/test/ex/TestingClassWithContext_.java")));
        final Compilation staticMetamodelCompilation = javac()
                .compile(JavaFileObjects.forSourceLines("cz.test.ex.TestingClassWithContext_",
                        readFileAsString(new File(OUTPUT_DIRECTORY + "/cz/test/ex/TestingClassWithContext_.java"))));

        assertThat(staticMetamodelCompilation).succeededWithoutWarnings();
    }
}

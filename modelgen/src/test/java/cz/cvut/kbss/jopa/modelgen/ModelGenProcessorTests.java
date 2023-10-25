/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.modelgen;

import com.google.testing.compile.Compilation;
import com.google.testing.compile.JavaFileObjects;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import static com.google.testing.compile.CompilationSubject.assertThat;
import static com.google.testing.compile.Compiler.javac;
import static org.junit.jupiter.api.Assertions.assertFalse;

class ModelGenProcessorTests {

    static String actualResult = "";

    @AfterAll
    static void deleteSMClass() {
        File file = new File("./target/generated-test-sources/static-metamodel/cz/test/ex/TestingClass_.java");
        file.delete();
    }

    @Test
    void createSMClassSuccess() throws Exception {
        String outputDirectory = "./target/generated-test-sources/static-metamodel";

        List<String> options = new ArrayList<>();
        options.add("-AoutputDirectory=" + outputDirectory);
        options.add("-AdebugOption=" + "true");
        Compilation compilation =
                javac()
                        .withProcessors(new ModelGenProcessor()).withOptions(options)
                        .compile(
                                JavaFileObjects.forSourceLines("cz.test.ex.TestingClassOWL", readFileAsString(
                                        new File("src/test/java/cz/test/ex/TestingClassOWL.java"))),
                                JavaFileObjects.forSourceLines("cz.test.ex.TestingClassNonEntity", readFileAsString(
                                        new File("src/test/java/cz/test/ex/TestingClassNonEntity.java"))),
                                JavaFileObjects.forSourceLines("cz.test.ex.TestingClassNotOWL", readFileAsString(
                                        new File("src/test/java/cz/test/ex/TestingClassNotOWL.java"))));
        assertThat(compilation).succeededWithoutWarnings();

        assertFalse(Files.exists(Paths.get(outputDirectory + "/cz/test/ex/TestingClassNonEntity_.java")));
        assertFalse(Files.exists(Paths.get(outputDirectory + "/cz/test/ex/TestingClassNotOWL_.java")));

        actualResult = readFileAsString(new File(outputDirectory + "/cz/test/ex/TestingClassOWL_.java"));
    }

    static String readFileAsString(File file) throws IOException {
        return String.join("\n", Files.readAllLines(file.toPath(), StandardCharsets.UTF_8));
    }
}

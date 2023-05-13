package cz.cvut.kbss.jopa.modelgen;

import com.google.testing.compile.Compilation;
import com.google.testing.compile.JavaFileObjects;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

import static com.google.testing.compile.CompilationSubject.assertThat;
import static com.google.testing.compile.Compiler.javac;
import static org.junit.jupiter.api.Assertions.assertFalse;

class ModelGenProcessorTests {

    static String actualResult = "";
    @Test
    void createSMClassSuccess(){
        String outputDirectory = "./target/generated-test-sources/static-metamodel";

        List<String> options = new ArrayList<>();
        options.add("-AoutputDirectory=" + outputDirectory);
        options.add("-AdebugOption=" + "true");
        Compilation compilation =
                javac()
                        .withProcessors(new ModelGenProcessor()).withOptions(options)
                        .compile(
                                JavaFileObjects.forSourceLines("cz.test.ex.TestingClassOWL",readFileAsString(new File("src/test/java/cz/test/ex/TestingClassOWL.java"))),
                                JavaFileObjects.forSourceLines("cz.test.ex.TestingClassNonEntity",readFileAsString(new File("src/test/java/cz/test/ex/TestingClassNonEntity.java"))),
                                JavaFileObjects.forSourceLines("cz.test.ex.TestingClassNotOWL",readFileAsString(new File("src/test/java/cz/test/ex/TestingClassNotOWL.java"))));
        assertThat(compilation).succeededWithoutWarnings();

        assertFalse(Files.exists(Paths.get(outputDirectory + "/cz/test/ex/TestingClassNonEntity_.java")));
        assertFalse(Files.exists(Paths.get(outputDirectory + "/cz/test/ex/TestingClassNotOWL_.java")));

        actualResult = readFileAsString(new File(outputDirectory + "/cz/test/ex/TestingClassOWL_.java"));
    }
    @AfterAll
    static void deleteSMClass() {
        File file = new File("./target/generated-test-sources/static-metamodel/cz/test/ex/TestingClass_.java");
        file.delete();
    }
    @Test
    void doesntContainInferred() {
        assertFalse(actualResult.isEmpty());
        assertFalse(actualResult.contains("public static volatile SingularAttribute<TestingClass, String> stringAttribute;"));
    }

    static String readFileAsString(File file) {
        try {
            Scanner scanner = new Scanner(file);
            String fileContents = scanner.useDelimiter("\\Z").next();
            scanner.close();
            return fileContents;
        } catch (IOException e) {
            System.err.println("Chyba cteni");
        }
        return "";
    }
}

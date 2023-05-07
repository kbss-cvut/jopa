package cz.cvut.kbss.jopa.modelgen;

import com.google.testing.compile.Compilation;
import com.google.testing.compile.JavaFileObjects;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

import static com.google.testing.compile.CompilationSubject.assertThat;
import static com.google.testing.compile.Compiler.javac;
import static org.junit.jupiter.api.Assertions.assertEquals;

class ModelGenProcessorTests {


    @Test
    void modelGenProcessorSuccessTest() {
        String outputDirectory = "./target/generated-test-sources/static-metamodel";

        List<String> options = new ArrayList<>();
        options.add("-AoutputDirectory=" + outputDirectory);
        options.add("-AdebugOption=" + "true");
        Compilation compilation =
                javac()
                        .withProcessors(new ModelGenProcessor()).withOptions(options)
                        .compile(JavaFileObjects.forSourceString(
                                "cz.test.ex.TestingClass", "package cz.test.ex;\n" +
                                        "\n" +
                                        "import cz.cvut.kbss.jopa.model.annotations.*;\n" +
                                        "\n" +
                                        "import java.net.URI;\n" +
                                        "import java.util.List;\n" +
                                        "import java.util.Map;\n" +
                                        "import java.util.Set;\n" +
                                        "\n" +
                                        "@MappedSuperclass\n" +
                                        "public class TestingClass {\n" +
                                        "    @Id(generated = true)\n" +
                                        "    private URI uri;\n" +
                                        "\n" +
                                        "    @OWLDataProperty(iri = \"http://krizik.felk.cvut.cz/ontologies/jopa/attributes#E-stringAttribute\")\n" +
                                        "    private String stringAttribute;\n" +
                                        "\n" +
                                        "    @OWLDataProperty(iri = \"http://krizik.felk.cvut.cz/ontologies/jopa/attributes#E-stringAttribute\")\n" +
                                        "    private TestingClass testingClass;\n" +
                                        "\n" +
                                        "    @Inferred\n" +
                                        "    @Properties(fetchType = FetchType.LAZY)\n" +
                                        "    private Map<String, Set<String>> properties;\n" +
                                        "    @Properties\n" +
                                        "    private Map<URI, Set<Object>> propertie;\n" +
                                        "    @OWLObjectProperty(iri = \"http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute\")\n" +
                                        "    private List<String> listAttribute;\n" +
                                        "    @OWLObjectProperty(iri = \"http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute\")\n" +
                                        "    private Set<String> setAttribute;" +
                                        "}"
                        ));
        assertThat(compilation).succeededWithoutWarnings();

        File resultFile = new File(outputDirectory + "/cz/test/ex/TestingClass_.java");
        assertEquals("package cz.test.ex;\n" +
                "\n" +
                "import cz.cvut.kbss.jopa.model.metamodel.*;\n" +
                "import javax.annotation.processing.Generated;\n" +
                "import cz.test.ex.TestingClass;\n" +
                "import java.net.URI;\n" +
                "import java.lang.String;\n" +
                "import java.util.Map;\n" +
                "import java.util.Set;\n" +
                "import java.util.List;\n" +
                "\n" +
                "@Generated(value = \"cz.cvut.kbss.jopa.modelgen.ModelGenProcessor\")\n" +
                "@StaticMetamodel(TestingClass.class)\n" +
                "public class TestingClass_ {\n" +
                "\n" +
                "\t public static volatile Identifier<TestingClass, URI> uri;\n" +
                "\t public static volatile SingularAttribute<TestingClass, String> stringAttribute;\n" +
                "\t public static volatile SingularAttribute<TestingClass, TestingClass> testingClass;\n" +
                "\t public static volatile PropertiesSpecification<TestingClass, Map, String, String> properties;\n" +
                "\t public static volatile PropertiesSpecification<TestingClass, Map, URI, Object> propertie;\n" +
                "\t public static volatile ListAttribute<TestingClass, String> listAttribute;\n" +
                "\t public static volatile SetAttribute<TestingClass, String> setAttribute;\n" +
                "}", readFileAsString(resultFile));
    }


    String readFileAsString(File file) {
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

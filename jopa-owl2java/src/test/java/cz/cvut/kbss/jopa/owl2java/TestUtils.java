package cz.cvut.kbss.jopa.owl2java;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

public class TestUtils {

    private TestUtils() {
        throw new AssertionError();
    }

    static final String MAPPING_FILE_NAME = "mapping";
    static final String IC_ONTOLOGY_IRI = "http://krizik.felk.cvut.cz/ontologies/owl2java-ics.owl";
    static final String CONTEXT = "owl2java-ic";

    static final String VOCABULARY_FILE = Constants.VOCABULARY_CLASS + ".java";

    static File getTempDirectory() throws IOException {
        final File targetDir = Files.createTempDirectory("owl2java-test").toFile();
        targetDir.deleteOnExit();
        return targetDir;
    }
}

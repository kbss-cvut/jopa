package cz.cvut.kbss.jopa.owl2java;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.io.File;
import java.util.Arrays;
import java.util.List;

import static cz.cvut.kbss.jopa.owl2java.TestUtils.MAPPING_FILE_NAME;
import static cz.cvut.kbss.jopa.owl2java.TestUtils.VOCABULARY_FILE;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class OWL2JavaTest {

    private String mappingFilePath;

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Before
    public void setUp() {
        this.mappingFilePath = resolveMappingFilePath();
    }

    private String resolveMappingFilePath() {
        final File mf = new File(getClass().getClassLoader().getResource(MAPPING_FILE_NAME).getFile());
        return mf.getAbsolutePath();
    }

    @Test
    public void vocabularyCommandGeneratesVocabularyFromICs() throws Exception {
        final File targetDir = TestUtils.getTempDirectory();
        final String[] args = new String[]{"vocabulary", TestUtils.IC_ONTOLOGY_IRI,
                "-m",
                mappingFilePath,
                "-c",
                TestUtils.CONTEXT,
                "-d",
                targetDir.getAbsolutePath()};
        OWL2Java.main(args);
        final List<String> fileNames = Arrays.asList(targetDir.list());
        assertEquals(1, fileNames.size());
        assertEquals(VOCABULARY_FILE, fileNames.get(0));
    }

    @Test
    public void transformGeneratesClassesAndVocabularyFromICs() throws Exception {
        final File targetDir = TestUtils.getTempDirectory();
        final String packageName = "cz.cvut.kbss.jopa.owl2java";
        final String[] args = new String[]{"transform", TestUtils.IC_ONTOLOGY_IRI,
                "-m",
                mappingFilePath,
                "-p",
                packageName,
                "-c",
                TestUtils.CONTEXT,
                "-d",
                targetDir.getAbsolutePath()};
        OWL2Java.main(args);
        assertTrue(targetDir.list().length > 0);
    }
}
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
package cz.cvut.kbss.jopa.owl2java;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static cz.cvut.kbss.jopa.owl2java.TestUtils.VOCABULARY_FILE;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasItem;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class OWL2JavaTest {

    private String mappingFilePath;

    private final ByteArrayOutputStream outContent = new ByteArrayOutputStream();
    private final ByteArrayOutputStream errContent = new ByteArrayOutputStream();

    private File targetDir;

    @BeforeEach
    public void setUp() throws Exception {
        this.mappingFilePath = TestUtils.resolveMappingFilePath();
        System.setOut(new PrintStream(outContent));
        System.setErr(new PrintStream(errContent));
    }

    @AfterEach
    public void tearDown() throws Exception {
        System.setOut(null);
        System.setErr(null);
        if (targetDir != null) {
            TestUtils.recursivelyDeleteDirectory(targetDir);
        }
    }

    @Test
    public void vocabularyCommandGeneratesVocabularyFromICs() throws Exception {
        this.targetDir = TestUtils.getTempDirectory();
        final String packageName = "";
        final String[] args = new String[]{"vocabulary", TestUtils.IC_ONTOLOGY_IRI,
                "-m",
                mappingFilePath,
                "-p",
                packageName,
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
        this.targetDir = TestUtils.getTempDirectory();
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
        assertThat(targetDir.list().length, greaterThan(0));
    }

    @Test
    public void transformPrintsErrorAndQuitsWhenContextIsMissing() throws Exception {
        this.targetDir = TestUtils.getTempDirectory();
        final String packageName = "cz.cvut.kbss.jopa.owl2java";
        final String[] args = new String[]{"transform", TestUtils.IC_ONTOLOGY_IRI,
                "-m",
                mappingFilePath,
                "-p",
                packageName,
                "-d",
                targetDir.getAbsolutePath()};
        OWL2Java.main(args);
        assertEquals(0, targetDir.list().length);
        verifyErrorContent("The parameter '-[a-z]' is obligatory");
    }

    private void verifyErrorContent(String regexp) {
        final String err = errContent.toString();
        final Pattern p = Pattern.compile(regexp);
        final Matcher m = p.matcher(err);
        assertTrue(m.find());
    }

    @Test
    public void transformPrintsErrorAndQuitesWhenOntologyIriIsMissing() throws Exception {
        this.targetDir = TestUtils.getTempDirectory();
        final String packageName = "cz.cvut.kbss.jopa.owl2java";
        final String[] args = new String[]{"transform",
                "-m",
                mappingFilePath,
                "-p",
                packageName,
                "-c",
                TestUtils.CONTEXT,
                "-d",
                targetDir.getAbsolutePath()};
        OWL2Java.main(args);
        assertEquals(0, targetDir.list().length);
        verifyErrorContent("Exactly one ontology IRI has to be specified, got 0");
    }

    @Test
    public void transformUsesCustomJavaClassNameSpecifiedByCustomAnnotation() throws Exception {
        this.targetDir = TestUtils.getTempDirectory();
        final String packageName = "";
        final String classNameAnnotation = "http://krizik.felk.cvut.cz/ontologies/2009/ic.owl#JavaClassName";
        final String className = "Company.java";
        final String[] args = new String[]{"transform", TestUtils.IC_ONTOLOGY_IRI,
                "-m",
                mappingFilePath,
                "-p",
                packageName,
                "-c",
                TestUtils.CONTEXT,
                "-jca",
                classNameAnnotation,
                "-d",
                targetDir.getAbsolutePath()};
        OWL2Java.main(args);
        final File modelDir = new File(targetDir.getAbsolutePath() + File.separator + "model");
        final Set<String> fileNames = new HashSet<>(Arrays.asList(modelDir.list()));
        assertThat(fileNames, hasItem(className));
    }
}

/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.owl2java;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.nio.file.Files;
import org.semanticweb.owlapi.model.OWLAxiom;

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

    static void addAxiom(OWLAxiom axiom, OWL2JavaTransformer transformer) throws Exception {
        final Field defaultContextField = OWL2JavaTransformer.class.getDeclaredField("DEFAULT_CONTEXT");
        defaultContextField.setAccessible(true);
        final ContextDefinition defaultContext = (ContextDefinition) defaultContextField.get(null);
        final Method addAxiomMethod = OWL2JavaTransformer.class
            .getDeclaredMethod("addAxiomToContext", ContextDefinition.class, OWLAxiom.class);
        addAxiomMethod.setAccessible(true);
        addAxiomMethod.invoke(transformer, defaultContext, axiom);
        final Method getContextMethod = OWL2JavaTransformer.class
            .getDeclaredMethod("getContextDefinition", String.class);
        getContextMethod.setAccessible(true);
        final ContextDefinition testContext = (ContextDefinition) getContextMethod.invoke(transformer, CONTEXT);
        addAxiomMethod.invoke(transformer, testContext, axiom);
        defaultContext.parse();
        testContext.parse();
    }
}

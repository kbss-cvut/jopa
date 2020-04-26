/**
 * Copyright (C) 2020 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.owl2java;

import com.fasterxml.jackson.annotation.JsonTypeInfo.Id;
import com.sun.codemodel.JAnnotationUse;
import com.sun.codemodel.JDefinedClass;
import com.sun.codemodel.JFieldVar;
import com.sun.codemodel.JType;

import cz.cvut.kbss.jopa.model.annotations.Types;
import cz.cvut.kbss.jopa.owl2java.config.TransformationConfiguration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

class JavaTransformerTest {

    private static final String ONTOLOGY_IRI = "http://onto.fel.cvut.cz/ontologies/owl2java/java-transformer-test";

    private OWLOntology ontology;

    private OWLDataFactory dataFactory;

    private JavaTransformer sut;

    @BeforeEach
    void setUp() throws Exception {
        final OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
        this.ontology = manager.createOntology(IRI.create(ONTOLOGY_IRI));
        this.dataFactory = new OWLDataFactoryImpl();
        this.sut = new JavaTransformer(TransformationConfiguration.builder().packageName("").build());
    }

    @Test
    void generatesValidJavaIdentifiersForIrisWithNonAsciiCharacters() {
        final IRI iri = IRI.create("http://onto.fel.cvut.cz/ontologies/slovník/agendový/popis-dat/pojem/navržený-term");
        ontology.add(dataFactory.getOWLDeclarationAxiom(dataFactory.getOWLClass(iri)));
        final ContextDefinition context = new ContextDefinition();
        context.add(dataFactory.getOWLClass(iri));
        final ObjectModel result = sut.generateVocabulary(ontology, context);
        final JDefinedClass vocabClass = result.getCodeModel()._getClass(Constants.VOCABULARY_CLASS);
        assertNotNull(vocabClass);
        final Map<String, JFieldVar> fields = vocabClass.fields();
        assertTrue(fields.keySet().stream().anyMatch(n -> n.endsWith("navrzeny_term")));
    }

    @Test
    void generateModelCreatesToStringMethodForGeneratedModelClasses() {
        final String className = "TestClass";
        final IRI iri = IRI.create("http://onto.fel.cvut.cz/ontologies/jopa/" + className);
        ontology.add(dataFactory.getOWLDeclarationAxiom(dataFactory.getOWLClass(iri)));
        final ContextDefinition context = new ContextDefinition();
        context.add(dataFactory.getOWLClass(iri));
        context.parse();
        final ObjectModel result = sut.generateModel(ontology, context);
        final JDefinedClass resultClass =
                result.getCodeModel()._getClass(Constants.MODEL_PACKAGE + Constants.PACKAGE_SEPARATOR + className);
        assertNotNull(resultClass);
        assertNotNull(resultClass.getMethod("toString", new JType[0]));
    }
    
    @Test
    void shouldGenerateIdAndTypesFields() {
        final String className = "TestClass";
        final IRI iri = IRI.create("http://onto.fel.cvut.cz/ontologies/jopa/" + className);
        ontology.add(dataFactory.getOWLDeclarationAxiom(dataFactory.getOWLClass(iri)));
        final ContextDefinition context = new ContextDefinition();
        context.add(dataFactory.getOWLClass(iri));
        context.parse();
        final ObjectModel result = sut.generateModel(ontology, context);
        final JDefinedClass resultClass =
                result.getCodeModel()._getClass(Constants.MODEL_PACKAGE + Constants.PACKAGE_SEPARATOR + className);
        checkHasFieldWithName(resultClass, "id");
        checkHasFieldWithName(resultClass, "types");
    }
    
    private void checkHasFieldWithName(final JDefinedClass cls, final String name) {
    	for(final JFieldVar f : cls.fields().values())
        	if (f.name().equals(name)) return;
        fail();    	
    }

}

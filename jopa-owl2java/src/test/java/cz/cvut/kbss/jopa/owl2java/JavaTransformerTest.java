/**
 * Copyright (C) 2020 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.owl2java;

import com.sun.codemodel.JDefinedClass;
import com.sun.codemodel.JFieldVar;
import com.sun.codemodel.JType;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.owl2java.config.TransformationConfiguration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.vocab.OWL2Datatype;
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class JavaTransformerTest {

    private static final String PREFIX = "http://onto.fel.cvut.cz/ontologies/jopa/";
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
        final IRI iri = IRI.create(PREFIX + className);
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
        final IRI iri = IRI.create(PREFIX + className);
        ontology.add(dataFactory.getOWLDeclarationAxiom(dataFactory.getOWLClass(iri)));
        final ContextDefinition context = new ContextDefinition();
        context.add(dataFactory.getOWLClass(iri));
        context.parse();
        final ObjectModel result = sut.generateModel(ontology, context);
        final JDefinedClass resultClass =
                result.getCodeModel()._getClass(Constants.MODEL_PACKAGE + Constants.PACKAGE_SEPARATOR + className);
        //Check the presence of fields with @Id and @Type annotation
        checkHasFieldWithName(resultClass, "id");
        checkHasFieldWithName(resultClass, "types");
    }

    private void checkHasFieldWithName(final JDefinedClass cls, final String name) {
        for (final JFieldVar f : cls.fields().values())
            if (f.name().equals(name)) return;
        fail();
    }

    @Test
    void shouldNotGenerateIdAndTypesFieldsInSubclasses() {
        final String className1 = "TestClass1";
        final IRI iri1 = IRI.create(PREFIX + className1);
        final String className2 = "TestClass2";
        final IRI iri2 = IRI.create(PREFIX + className2);

        ontology.add(dataFactory.getOWLDeclarationAxiom(dataFactory.getOWLClass(iri1)));
        ontology.add(dataFactory.getOWLDeclarationAxiom(dataFactory.getOWLClass(iri2)));
        final ContextDefinition context = new ContextDefinition();
        final OWLClass owlClass1 = dataFactory.getOWLClass(iri1);
        final OWLClass owlClass2 = dataFactory.getOWLClass(iri2);
        context.add(owlClass1);
        context.add(owlClass2);
        context.addAxiom(dataFactory.getOWLSubClassOfAxiom(owlClass2, owlClass1));
        context.parse();
        final ObjectModel result = sut.generateModel(ontology, context);
        final JDefinedClass resultClass1 =
                result.getCodeModel()._getClass(Constants.MODEL_PACKAGE + Constants.PACKAGE_SEPARATOR + className1);
        //Check the presence of fields with @Id and @Type annotation
        checkHasFieldWithName(resultClass1, "id");
        checkHasFieldWithName(resultClass1, "types");
        final JDefinedClass resultClass2 =
                result.getCodeModel()._getClass(Constants.MODEL_PACKAGE + Constants.PACKAGE_SEPARATOR + className2);
        // Check the absence of fields with @Id and @Type annotation
        checkHasNoFieldWithName(resultClass2, "id");
        checkHasNoFieldWithName(resultClass2, "types");
    }

    private void checkHasNoFieldWithName(final JDefinedClass cls, final String name) {
        for (final JFieldVar f : cls.fields().values())
            assertNotEquals(f.name(), name);
    }

    @Test
    void generateModelGeneratesFieldOfTypeMultilingualStringForLangStringRangeWhenConfiguredToPreferMultilingualStrings() {
        final String className = "TestClass1";
        final String fieldName = "multilingualString";
        final ContextDefinition context = generateAxiomsForLangStrings(className, fieldName);
        final ObjectModel result = sut.generateModel(ontology, context);
        final JDefinedClass resultClass =
                result.getCodeModel()._getClass(Constants.MODEL_PACKAGE + Constants.PACKAGE_SEPARATOR + className);
        assertNotNull(resultClass);
        final JFieldVar resultField = resultClass.fields().get(fieldName);
        assertNotNull(resultField);
        assertEquals(MultilingualString.class.getCanonicalName(), resultField.type().fullName());
    }

    private ContextDefinition generateAxiomsForLangStrings(String className, String fieldName) {
        final IRI classIri = IRI.create(PREFIX + className);
        ontology.add(dataFactory.getOWLDeclarationAxiom(dataFactory.getOWLClass(classIri)));
        final IRI propertyIri = IRI.create(PREFIX + fieldName);
        ontology.add(dataFactory.getOWLDeclarationAxiom(dataFactory.getOWLDataProperty(propertyIri)));
        ontology.add(dataFactory.getOWLDataPropertyRangeAxiom(dataFactory.getOWLDataProperty(propertyIri),
                OWL2Datatype.RDF_LANG_STRING));
        final ContextDefinition context = new ContextDefinition();
        context.add(dataFactory.getOWLClass(classIri));
        context.add(dataFactory.getOWLDataProperty(propertyIri));
        context.addAxiom(dataFactory.getOWLSubClassOfAxiom(dataFactory.getOWLClass(classIri), dataFactory
                .getOWLDataAllValuesFrom(dataFactory.getOWLDataProperty(propertyIri), OWL2Datatype.RDF_LANG_STRING)));
        context.addAxiom(dataFactory.getOWLSubClassOfAxiom(dataFactory.getOWLClass(classIri), dataFactory
                .getOWLDataMaxCardinality(1, dataFactory.getOWLDataProperty(propertyIri))));
        context.parse();
        return context;
    }

    @Test
    void generateModelGeneratesFieldOfTypeStringForLangStringRangeWhenConfiguredNotToPreferMultilingualStrings() {
        this.sut = new JavaTransformer(TransformationConfiguration.builder().preferMultilingualStrings(false).packageName("").build());
        final String className = "TestClass";
        final String fieldName = "multilingualString";
        final ContextDefinition context = generateAxiomsForLangStrings(className, fieldName);
        final ObjectModel result = sut.generateModel(ontology, context);
        final JDefinedClass resultClass =
                result.getCodeModel()._getClass(Constants.MODEL_PACKAGE + Constants.PACKAGE_SEPARATOR + className);
        assertNotNull(resultClass);
        final JFieldVar resultField = resultClass.fields().get(fieldName);
        assertNotNull(resultField);
        assertEquals(String.class.getCanonicalName(), resultField.type().fullName());
    }
}

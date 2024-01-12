/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.owl2java;

import com.sun.codemodel.JDefinedClass;
import com.sun.codemodel.JFieldVar;
import com.sun.codemodel.JType;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.owl2java.config.Defaults;
import cz.cvut.kbss.jopa.owl2java.config.TransformationConfiguration;
import cz.cvut.kbss.jopa.vocabulary.RDFS;
import cz.cvut.kbss.jopa.vocabulary.SKOS;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.util.OWLOntologyMerger;
import org.semanticweb.owlapi.vocab.OWL2Datatype;
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasKey;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

class JavaTransformerTest {

    private static final String PREFIX = "http://onto.fel.cvut.cz/ontologies/jopa/";
    private static final String ONTOLOGY_IRI = "http://onto.fel.cvut.cz/ontologies/owl2java/java-transformer-test/";

    private OWLOntologyManager ontologyManager;

    private OWLOntology ontology;

    private OWLDataFactory dataFactory;

    private JavaTransformer sut;

    @BeforeEach
    void setUp() throws Exception {
        this.ontologyManager = OWLManager.createOWLOntologyManager();
        this.ontology = ontologyManager.createOntology(IRI.create(ONTOLOGY_IRI));
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
        for (final JFieldVar f : cls.fields().values()) {if (f.name().equals(name)) {return;}}
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
        for (final JFieldVar f : cls.fields().values()) {assertNotEquals(f.name(), name);}
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
        this.sut = new JavaTransformer(TransformationConfiguration.builder().preferMultilingualStrings(false)
                                                                  .packageName("").build());
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

    @Test
    void generateModelGeneratesJavadocWithCommentInDefaultLanguage() {
        final String className = "TestClass";
        final IRI iri = IRI.create(PREFIX + className);
        final OWLClass owlClass = dataFactory.getOWLClass(iri);
        ontology.add(dataFactory.getOWLDeclarationAxiom(owlClass));
        final String expectedComment = "Comment in default language";
        ontology.add(dataFactory.getOWLAnnotationAssertionAxiom(dataFactory.getOWLAnnotationProperty(RDFS.COMMENT), owlClass.getIRI(), dataFactory.getOWLLiteral("Český komentář", "cs")));
        ontology.add(dataFactory.getOWLAnnotationAssertionAxiom(dataFactory.getOWLAnnotationProperty(RDFS.COMMENT), owlClass.getIRI(), dataFactory.getOWLLiteral(expectedComment, Constants.LANGUAGE)));
        final ContextDefinition context = new ContextDefinition();
        context.add(dataFactory.getOWLClass(iri));
        context.parse();
        final ObjectModel result = sut.generateModel(ontology, context);
        final JDefinedClass resultClass =
                result.getCodeModel()._getClass(Constants.MODEL_PACKAGE + Constants.PACKAGE_SEPARATOR + className);
        assertThat(resultClass.javadoc().toString(), containsString(expectedComment));
    }

    @Test
    void generateModelGeneratesJavadocWithCommentUsingLanguageLessAnnotationValue() {
        final String className = "TestClass";
        final IRI iri = IRI.create(PREFIX + className);
        final OWLClass owlClass = dataFactory.getOWLClass(iri);
        ontology.add(dataFactory.getOWLDeclarationAxiom(owlClass));
        final String expectedComment = "Comment in default language";
        ontology.add(dataFactory.getOWLAnnotationAssertionAxiom(dataFactory.getOWLAnnotationProperty(RDFS.COMMENT), owlClass.getIRI(), dataFactory.getOWLLiteral(expectedComment)));
        final ContextDefinition context = new ContextDefinition();
        context.add(dataFactory.getOWLClass(iri));
        context.parse();
        final ObjectModel result = sut.generateModel(ontology, context);
        final JDefinedClass resultClass =
                result.getCodeModel()._getClass(Constants.MODEL_PACKAGE + Constants.PACKAGE_SEPARATOR + className);
        assertThat(resultClass.javadoc().toString(), containsString(expectedComment));
    }

    @Test
    void generateModelDoesNotGenerateLabelAndDescriptionFieldsWhenDisabled() {
        this.sut = new JavaTransformer(TransformationConfiguration.builder().packageName("")
                                                                  .generateAnnotationFields(false).build());
        final String className = "TestClass";
        final IRI iri = IRI.create(PREFIX + className);
        final OWLClass owlClass = dataFactory.getOWLClass(iri);
        ontology.add(dataFactory.getOWLDeclarationAxiom(owlClass));
        final ContextDefinition context = new ContextDefinition();
        context.add(dataFactory.getOWLClass(iri));
        context.parse();
        final ObjectModel result = sut.generateModel(ontology, context);
        final JDefinedClass resultClass =
                result.getCodeModel()._getClass(Constants.MODEL_PACKAGE + Constants.PACKAGE_SEPARATOR + className);
        final Map<String, JFieldVar> fields = resultClass.fields();
        assertThat(fields, not(hasKey(Constants.LABEL_FIELD_NAME)));
        assertThat(fields, not(hasKey(Constants.DESCRIPTION_FIELD_NAME)));
    }

    @Test
    void generateModelDoesNotGenerateThingEntityClassWhenDisabled() {
        this.sut = new JavaTransformer(TransformationConfiguration.builder().packageName("").generateThing(false)
                                                                  .build());
        final ContextDefinition context = new ContextDefinition();
        context.parse();
        final ObjectModel result = sut.generateModel(ontology, context);
        final JDefinedClass resultClass =
                result.getCodeModel()._getClass(Constants.MODEL_PACKAGE + Constants.PACKAGE_SEPARATOR + "Thing");
        assertNull(resultClass);
    }

    @Test
    void generateModelDisambiguateClassesWithIdenticalJavaName() {
        this.sut = new JavaTransformer(TransformationConfiguration.builder().packageName("").generateThing(false)
                                                                  .build());
        final String className = "Concept";
        final IRI iriOne = IRI.create(PREFIX + className);
        final IRI iriTwo = IRI.create(SKOS.CONCEPT);
        ontology.add(dataFactory.getOWLDeclarationAxiom(dataFactory.getOWLClass(iriOne)));
        ontology.add(dataFactory.getOWLDeclarationAxiom(dataFactory.getOWLClass(iriTwo)));
        final ContextDefinition context = new ContextDefinition();
        context.add(dataFactory.getOWLClass(iriOne));
        context.add(dataFactory.getOWLClass(iriTwo));
        context.parse();

        final ObjectModel result = sut.generateModel(ontology, context);
        final Iterator<JDefinedClass> classIterator = result.getCodeModel()._package(Constants.MODEL_PACKAGE).classes();
        final List<JDefinedClass> classes = new ArrayList<>();
        while (classIterator.hasNext()) {
            classes.add(classIterator.next());
        }
        assertEquals(2, classes.size());
        assertEquals(2, classes.stream().filter(cls -> cls.name().endsWith(className)).count());
    }

    @Test
    void generateVocabularyDisambiguatePropertiesWithSameNameUsingPrefix() throws Exception {
        final OWLAnnotationProperty prefixProperty = dataFactory.getOWLAnnotationProperty(Defaults.ONTOLOGY_PREFIX_PROPERTY);
        ontology.add(dataFactory.getOWLAnnotationAssertionAxiom(prefixProperty, IRI.create(ONTOLOGY_IRI), dataFactory.getOWLLiteral("test")));
        final IRI personIri = IRI.create(PREFIX + "Person");
        ontology.add(dataFactory.getOWLDeclarationAxiom(dataFactory.getOWLClass(personIri)));
        final ContextDefinition context = new ContextDefinition();
        context.add(dataFactory.getOWLClass(personIri));
        final IRI foafOntIri = IRI.create("http://xmlns.com/foaf/0.1/");
        final OWLOntology foafOnto = ontology.getOWLOntologyManager().createOntology(foafOntIri);
        foafOnto.add(dataFactory.getOWLAnnotationAssertionAxiom(prefixProperty, foafOntIri, dataFactory.getOWLLiteral("foaf")));
        final IRI foafPersonIri = IRI.create(foafOntIri.getIRIString() + "Person");
        foafOnto.add(dataFactory.getOWLDeclarationAxiom(dataFactory.getOWLClass(foafPersonIri)));
        context.add(dataFactory.getOWLClass(foafPersonIri));
        final OWLOntology merged = new OWLOntologyMerger(ontologyManager).createMergedOntology(ontologyManager, null);

        final ObjectModel result = sut.generateVocabulary(merged, context);
        final JDefinedClass vocabClass = result.getCodeModel()._getClass(Constants.VOCABULARY_CLASS);
        assertNotNull(vocabClass);
        final Map<String, JFieldVar> fields = vocabClass.fields();
        assertThat(fields.keySet(), hasItem("s_c_Person"));
        assertThat(fields.keySet(), hasItem("s_c_foaf_Person"));
    }

    @Test
    void generateVocabularyUsesOntologyPrefixesInFieldNamesWhenGeneratingOntologyIriConstants() throws Exception {
        final ContextDefinition context = new ContextDefinition();
        final OWLAnnotationProperty prefixProperty = dataFactory.getOWLAnnotationProperty(Defaults.ONTOLOGY_PREFIX_PROPERTY);
        ontology.add(dataFactory.getOWLAnnotationAssertionAxiom(prefixProperty, IRI.create(ONTOLOGY_IRI), dataFactory.getOWLLiteral("test")));
        final IRI foafOntIri = IRI.create("http://xmlns.com/foaf/0.1/");
        final OWLOntology foafOnto = ontology.getOWLOntologyManager().createOntology(foafOntIri);
        foafOnto.add(dataFactory.getOWLAnnotationAssertionAxiom(prefixProperty, foafOntIri, dataFactory.getOWLLiteral("foaf")));
        final OWLOntology merged = new OWLOntologyMerger(ontologyManager).createMergedOntology(ontologyManager, null);

        final ObjectModel result = sut.generateVocabulary(merged, context);
        final JDefinedClass vocabClass = result.getCodeModel()._getClass(Constants.VOCABULARY_CLASS);
        assertNotNull(vocabClass);
        final Map<String, JFieldVar> fields = vocabClass.fields();
        assertThat(fields.keySet(), hasItem("ONTOLOGY_IRI_TEST"));
        assertThat(fields.keySet(), hasItem("ONTOLOGY_IRI_FOAF"));
    }

    @Test
    void generateModelDisambiguateClassesWithIdenticalJavaNameUsingOntologyPrefixes() throws Exception {
        this.sut = new JavaTransformer(TransformationConfiguration.builder().packageName("").generateThing(false)
                                                                  .build());
        final String className = "Concept";
        final IRI iriOne = IRI.create(PREFIX + className);
        final IRI iriTwo = IRI.create(SKOS.CONCEPT);
        ontology.add(dataFactory.getOWLDeclarationAxiom(dataFactory.getOWLClass(iriOne)));
        final IRI skosOntIri = IRI.create(SKOS.NAMESPACE);
        final OWLOntology skosOnto = ontology.getOWLOntologyManager().createOntology(skosOntIri);
        final OWLAnnotationProperty prefixProperty = dataFactory.getOWLAnnotationProperty(Defaults.ONTOLOGY_PREFIX_PROPERTY);
        skosOnto.add(dataFactory.getOWLAnnotationAssertionAxiom(prefixProperty, IRI.create(SKOS.NAMESPACE), dataFactory.getOWLLiteral(SKOS.PREFIX)));
        skosOnto.add(dataFactory.getOWLDeclarationAxiom(dataFactory.getOWLClass(iriTwo)));
        final OWLOntology merged = new OWLOntologyMerger(ontologyManager).createMergedOntology(ontologyManager, null);
        final ContextDefinition context = new ContextDefinition();
        context.add(dataFactory.getOWLClass(iriOne));
        context.add(dataFactory.getOWLClass(iriTwo));
        context.parse();

        final ObjectModel result = sut.generateModel(merged, context);
        final Iterator<JDefinedClass> classIterator = result.getCodeModel()._package(Constants.MODEL_PACKAGE).classes();
        final List<JDefinedClass> classes = new ArrayList<>();
        while (classIterator.hasNext()) {
            classes.add(classIterator.next());
        }
        assertEquals(2, classes.size());
        assertEquals(2, classes.stream().filter(cls -> cls.name().endsWith(className)).count());
        assertTrue(classes.stream().anyMatch(cls -> cls.name().equals("Skos" + className)));
    }
}

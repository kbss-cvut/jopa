/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.modelgen;

import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.modelgen.classmodel.Field;
import cz.cvut.kbss.jopa.modelgen.classmodel.MappingAnnotation;
import cz.cvut.kbss.jopa.modelgen.classmodel.MetamodelClass;
import cz.cvut.kbss.jopa.modelgen.classmodel.Type;
import cz.test.ex.TestingClassOWL;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static cz.cvut.kbss.jopa.modelgen.ModelGenProcessorTests.deleteTestFile;
import static cz.cvut.kbss.jopa.modelgen.ModelGenProcessorTests.readFileAsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;


class OutputFilesGeneratorTests {

    private static final String OUTPUT_FILE = "./src/test/java/cz/test/ex/TestingClassOWL_.java";

    private MetamodelClass metamodelClass;
    private Field objectProperty;
    private Field dataListProperty;
    private Field dataSetProperty;

    @BeforeEach
    void createSMClass() {
        this.metamodelClass = new MetamodelClass();
        Class<TestingClassOWL> testingClass = TestingClassOWL.class;
        metamodelClass.setName(testingClass.getSimpleName());
        metamodelClass.setPckg(testingClass.getPackage().getName());
        metamodelClass.getImports().add(testingClass.getName());
        metamodelClass.setSuperClass("");
        metamodelClass.setClassIri(TestingClassOWL.class.getAnnotation(OWLClass.class).iri());

        Field idField = new Field();
        idField.setAnnotatedWith(MappingAnnotation.ID);
        idField.setName("uri");
        idField.setParentName("cz.test.ex.TestingClassOWL");
        Type idType = new Type(URI.class.getName(), URI.class.getSimpleName());
        idField.setType(idType);
        metamodelClass.addField(idField);

        this.objectProperty = new Field();
        objectProperty.setAnnotatedWith(MappingAnnotation.OBJECT_PROPERTY);
        objectProperty.setName("object");
        objectProperty.setParentName("cz.test.ex.TestingClassOWL");
        objectProperty.setPropertyIri(TestingClassOWL.field("testingClassOWL").getAnnotation(OWLObjectProperty.class)
                                                     .iri());
        objectProperty.setType(new Type(TestingClassOWL.class.getName(), TestingClassOWL.class.getSimpleName()));
        metamodelClass.addField(objectProperty);

        Field typesField = new Field();
        typesField.setAnnotatedWith(MappingAnnotation.TYPES);
        typesField.setName("types");
        typesField.setParentName("cz.test.ex.TestingClassOWL");
        typesField.setType(idType);
        metamodelClass.addField(typesField);

        this.dataListProperty = new Field();
        dataListProperty.setAnnotatedWith(MappingAnnotation.DATA_PROPERTY);
        dataListProperty.setName("dataList");
        dataListProperty.setParentName("cz.test.ex.TestingClassOWL");
        dataListProperty.setPropertyIri(TestingClassOWL.field("listAttribute").getAnnotation(OWLDataProperty.class)
                                                       .iri());
        Type listType = new Type(List.class.getName(), List.class.getSimpleName());
        listType.setIsSimple(false);
        dataListProperty.setType(listType);
        listType.setTypes(Collections.singletonList(new Type(String.class.getName(), String.class.getSimpleName())));
        dataListProperty.setType(listType);
        metamodelClass.addField(dataListProperty);

        this.dataSetProperty = new Field();
        dataSetProperty.setAnnotatedWith(MappingAnnotation.ANNOTATION_PROPERTY);
        dataSetProperty.setName("dataSet");
        dataSetProperty.setParentName("cz.test.ex.TestingClassOWL");
        dataSetProperty.setPropertyIri(TestingClassOWL.field("setAttribute").getAnnotation(OWLAnnotationProperty.class)
                                                      .iri());
        Type setType = new Type(Set.class.getName(), Set.class.getSimpleName());
        setType.setIsSimple(false);
        dataSetProperty.setType(setType);
        setType.setTypes(Collections.singletonList(new Type(String.class.getName(), String.class.getSimpleName())));
        dataSetProperty.setType(setType);
        metamodelClass.addField(dataSetProperty);

        Field propertiesField = new Field();
        propertiesField.setAnnotatedWith(MappingAnnotation.PROPERTIES);
        propertiesField.setName("properties");
        propertiesField.setParentName("cz.test.ex.TestingClassOWL");
        final Type propsType = new Type(Map.class.getName(), Map.class.getSimpleName());
        propertiesField.setType(propsType);
        propsType.setTypes(List.of(new Type(String.class.getName(), String.class.getSimpleName()), setType));
        metamodelClass.addField(propertiesField);
    }

    @Nested
    class BasicGeneratedOutputTest {

        @AfterEach
        void tearDown() throws IOException {
            deleteTestFile(OUTPUT_FILE);
        }

        @Test
        void containsAllMetamodelFields() throws IOException {
            new OutputFilesGenerator(new OutputConfig("./src/test/java", false, false, false), false, null).generateOutputFiles(List.of(metamodelClass));
            String actualResult = readFileAsString(new File(OUTPUT_FILE));

            assertThat(actualResult, containsString("public static volatile Identifier<TestingClassOWL, URI> uri;"));
            assertThat(actualResult, containsString("public static volatile SingularAttribute<TestingClassOWL, TestingClassOWL> object;"));
            assertThat(actualResult, containsString("public static volatile ListAttribute<TestingClassOWL, String> dataList;"));
            assertThat(actualResult, containsString("public static volatile SetAttribute<TestingClassOWL, String> dataSet;"));
            assertThat(actualResult, containsString("public static volatile PropertiesSpecification<TestingClassOWL, Map, String, String> properties;"));
        }

        @Test
        void containsClassIriField() throws IOException {
            new OutputFilesGenerator(new OutputConfig("./src/test/java", false, false, false), false, null).generateOutputFiles(List.of(metamodelClass));
            String actualResult = readFileAsString(new File(OUTPUT_FILE));

            assertThat(actualResult, containsString("public static volatile IRI entityClassIRI;"));
        }
    }

    @Nested
    class GeneratedOutputWithPropertyIrisTest {

        @AfterEach
        void tearDown() throws IOException {
            deleteTestFile(OUTPUT_FILE);
        }

        @Test
        void containsPropertyIris() throws IOException {
            new OutputFilesGenerator(new OutputConfig("./src/test/java", true, false, false), false, null).generateOutputFiles(List.of(metamodelClass));
            String actualResult = readFileAsString(new File(OUTPUT_FILE));

            assertThat(actualResult, containsString("public static volatile IRI objectPropertyIRI;"));
            assertThat(actualResult, containsString("public static volatile IRI dataListPropertyIRI;"));
            assertThat(actualResult, containsString("public static volatile IRI dataSetPropertyIRI;"));
        }
    }

    @Nested
    class GeneratedOutputWithIrisAsStringsTest {

        @AfterEach
        void tearDown() throws IOException {
            deleteTestFile(OUTPUT_FILE);
        }

        @Test
        void containsIrisAsStrings() throws IOException {
            new OutputFilesGenerator(new OutputConfig("./src/test/java", true, true, false), false, null).generateOutputFiles(List.of(metamodelClass));
            String actualResult = readFileAsString(new File(OUTPUT_FILE));

            assertThat(actualResult, containsString("public static volatile String entityClassIRI;"));
            assertThat(actualResult, containsString("public static volatile String objectPropertyIRI;"));
            assertThat(actualResult, containsString("public static volatile String dataListPropertyIRI;"));
            assertThat(actualResult, containsString("public static volatile String dataSetPropertyIRI;"));
        }
    }

    @Nested
    class GenerateOutputWithInitializedIriFieldsTest {

        @AfterEach
        void tearDown() throws IOException {
            deleteTestFile(OUTPUT_FILE);
        }

        @Test
        void containsClassIriFieldWithFinalStringValue() throws IOException {
            new OutputFilesGenerator(new OutputConfig("./src/test/java", false, true, true), false, null).generateOutputFiles(List.of(metamodelClass));
            String actualResult = readFileAsString(new File(OUTPUT_FILE));

            assertThat(actualResult, containsString("public static final String entityClassIRI = \"" + metamodelClass.getClassIri() + "\";"));
        }

        @Test
        void containsClassIriFieldWithFinalIriValue() throws IOException {
            new OutputFilesGenerator(new OutputConfig("./src/test/java", false, false, true), false, null).generateOutputFiles(List.of(metamodelClass));
            String actualResult = readFileAsString(new File(OUTPUT_FILE));

            assertThat(actualResult, containsString("import cz.cvut.kbss.jopa.model.IRI;"));
            assertThat(actualResult, containsString("public static final IRI entityClassIRI = IRI.create(\"" + metamodelClass.getClassIri() + "\");"));
        }

        @Test
        void containsPropertyIriFieldsWithFinalStringValues() throws IOException {
            new OutputFilesGenerator(new OutputConfig("./src/test/java", true, true, true), false, null).generateOutputFiles(List.of(metamodelClass));
            String actualResult = readFileAsString(new File(OUTPUT_FILE));

            assertThat(actualResult, containsString("public static final String objectPropertyIRI = \"" + objectProperty.getPropertyIri() + "\";"));
            assertThat(actualResult, containsString("public static final String dataListPropertyIRI = \"" + dataListProperty.getPropertyIri() + "\";"));
            assertThat(actualResult, containsString("public static final String dataSetPropertyIRI = \"" + dataSetProperty.getPropertyIri() + "\";"));
        }

        @Test
        void containsPropertyIriFieldsWithFinalIriValues() throws IOException {
            new OutputFilesGenerator(new OutputConfig("./src/test/java", true, false, true), false, null).generateOutputFiles(List.of(metamodelClass));
            String actualResult = readFileAsString(new File(OUTPUT_FILE));

            assertThat(actualResult, containsString("import cz.cvut.kbss.jopa.model.IRI;"));
            assertThat(actualResult, containsString("public static final IRI objectPropertyIRI = IRI.create(\"" + objectProperty.getPropertyIri() + "\");"));
            assertThat(actualResult, containsString("public static final IRI dataListPropertyIRI = IRI.create(\"" + dataListProperty.getPropertyIri() + "\");"));
            assertThat(actualResult, containsString("public static final IRI dataSetPropertyIRI = IRI.create(\"" + dataSetProperty.getPropertyIri() + "\");"));
        }
    }
}

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
import java.util.Collections;
import java.util.List;
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

        Field field1 = new Field();
        field1.setAnnotatedWith(MappingAnnotation.ID);
        field1.setName("uri");
        field1.setParentName("cz.test.ex.TestingClassOWL");
        Type type1 = new Type();
        type1.setTypeName("Test");
        type1.setSimpleName("Test");
        field1.setType(type1);
        metamodelClass.addField(field1);

        this.objectProperty = new Field();
        objectProperty.setAnnotatedWith(MappingAnnotation.OBJECT_PROPERTY);
        objectProperty.setName("object");
        objectProperty.setParentName("cz.test.ex.TestingClassOWL");
        objectProperty.setPropertyIri(TestingClassOWL.field("testingClassOWL").getAnnotation(OWLObjectProperty.class)
                                                     .iri());
        objectProperty.setType(type1);
        metamodelClass.addField(objectProperty);

        Field field3 = new Field();
        field3.setAnnotatedWith(MappingAnnotation.TYPES);
        field3.setName("types");
        field3.setParentName("cz.test.ex.TestingClassOWL");
        field3.setType(type1);
        metamodelClass.addField(field3);

        this.dataListProperty = new Field();
        dataListProperty.setAnnotatedWith(MappingAnnotation.DATA_PROPERTY);
        dataListProperty.setName("dataList");
        dataListProperty.setParentName("cz.test.ex.TestingClassOWL");
        dataListProperty.setPropertyIri(TestingClassOWL.field("listAttribute").getAnnotation(OWLDataProperty.class)
                                                       .iri());
        Type type5 = new Type();
        type5.setTypeName(List.class.getName());
        type5.setSimpleName(List.class.getSimpleName());
        type5.setIsSimple(false);
        dataListProperty.setType(type5);
        type5.setTypes(Collections.singletonList(type1));
        dataListProperty.setType(type5);
        metamodelClass.addField(dataListProperty);

        this.dataSetProperty = new Field();
        dataSetProperty.setAnnotatedWith(MappingAnnotation.ANNOTATION_PROPERTY);
        dataSetProperty.setName("dataSet");
        dataSetProperty.setParentName("cz.test.ex.TestingClassOWL");
        dataSetProperty.setPropertyIri(TestingClassOWL.field("setAttribute").getAnnotation(OWLAnnotationProperty.class)
                                                      .iri());
        Type type6 = new Type();
        type6.setTypeName(Set.class.getName());
        type6.setSimpleName(Set.class.getSimpleName());
        type6.setIsSimple(false);
        dataSetProperty.setType(type6);
        type6.setTypes(Collections.singletonList(type1));
        dataSetProperty.setType(type6);
        metamodelClass.addField(dataSetProperty);

        Field field8 = new Field();
        field8.setAnnotatedWith(MappingAnnotation.PROPERTIES);
        field8.setName("properties");
        field8.setParentName("cz.test.ex.TestingClassOWL");
        field8.setType(type6);
        type6.setTypes(Collections.singletonList(type1));
        field8.setType(type6);
        metamodelClass.addField(field8);
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

            assertThat(actualResult, containsString("public static volatile Identifier<TestingClassOWL, Test> uri;"));
            assertThat(actualResult, containsString("public static volatile SingularAttribute<TestingClassOWL, Test> object;"));
            assertThat(actualResult, containsString("public static volatile ListAttribute<TestingClassOWL, Test> dataList;"));
            assertThat(actualResult, containsString("public static volatile SetAttribute<TestingClassOWL, Test> dataSet;"));
            assertThat(actualResult, containsString("public static volatile PropertiesSpecification<TestingClassOWL, Test> properties;"));
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

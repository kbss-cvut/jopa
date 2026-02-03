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

import cz.cvut.kbss.jopa.modelgen.classmodel.Field;
import cz.cvut.kbss.jopa.modelgen.classmodel.MappingAnnotations;
import cz.cvut.kbss.jopa.modelgen.classmodel.MetamodelClass;
import cz.cvut.kbss.jopa.modelgen.classmodel.Type;
import cz.test.ex.TestingClassOWL;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import static cz.cvut.kbss.jopa.modelgen.ModelGenProcessorTests.readFileAsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;


class OutputFilesGeneratorTests {

    private static final String OUTPUT_FILE = "./src/test/java/cz/test/ex/TestingClassOWL_.java";

    private static MetamodelClass metamodelClass;

    @BeforeAll
    static void createSMClass() {
        metamodelClass = new MetamodelClass();
        Class<TestingClassOWL> testingClass = TestingClassOWL.class;
        metamodelClass.setName(testingClass.getSimpleName());
        metamodelClass.setPckg(testingClass.getPackage().getName());
        metamodelClass.getImports().add(testingClass.getName());
        metamodelClass.makeEntityClass();
        metamodelClass.setExtend("");

        Field field1 = new Field();
        field1.setAnnotatedWith(Collections.singletonList(MappingAnnotations.ID));
        field1.setName("uri");
        field1.setParentName("cz.test.ex.TestingClassOWL");
        Type type1 = new Type();
        type1.setTypeName("Test");
        type1.setSimpleName("Test");
        field1.setType(type1);
        metamodelClass.addField(field1);

        Field field2 = new Field();
        field2.setAnnotatedWith(Collections.singletonList(MappingAnnotations.OBJECT_PROPERTY));
        field2.setName("object");
        field2.setParentName("cz.test.ex.TestingClassOWL");
        field2.setType(type1);
        metamodelClass.addField(field2);

        Field field3 = new Field();
        field3.setAnnotatedWith(Collections.singletonList(MappingAnnotations.TYPES));
        field3.setName("types");
        field3.setParentName("cz.test.ex.TestingClassOWL");
        field3.setType(type1);
        metamodelClass.addField(field3);

        Field field5 = new Field();
        field5.setAnnotatedWith(Collections.singletonList(MappingAnnotations.DATA_PROPERTY));
        field5.setName("dataList");
        field5.setParentName("cz.test.ex.TestingClassOWL");
        Type type5 = new Type();
        type5.setTypeName(List.class.getName());
        type5.setSimpleName(List.class.getSimpleName());
        type5.setIsSimple(false);
        field5.setType(type5);
        type5.setTypes(Collections.singletonList(type1));
        field5.setType(type5);
        metamodelClass.addField(field5);

        Field field6 = new Field();
        field6.setAnnotatedWith(Collections.singletonList(MappingAnnotations.DATA_PROPERTY));
        field6.setName("dataSet");
        field6.setParentName("cz.test.ex.TestingClassOWL");
        Type type6 = new Type();
        type6.setTypeName(Set.class.getName());
        type6.setSimpleName(Set.class.getSimpleName());
        type6.setIsSimple(false);
        field6.setType(type6);
        type6.setTypes(Collections.singletonList(type1));
        field6.setType(type6);
        metamodelClass.addField(field6);

        Field field8 = new Field();
        field8.setAnnotatedWith(Collections.singletonList(MappingAnnotations.PROPERTIES));
        field8.setName("properties");
        field8.setParentName("cz.test.ex.TestingClassOWL");
        field8.setType(type6);
        type6.setTypes(Collections.singletonList(type1));
        field8.setType(type6);
        metamodelClass.addField(field8);
    }

    @Test
    void isAnnotatedWithSuccessTest() {
        Field field = new Field();
        assertFalse(OutputFilesGenerator.isAnnotatedWith(field, null));

        field.setAnnotatedWith(MappingAnnotations.getAll());

        assertTrue(OutputFilesGenerator.isAnnotatedWith(field, MappingAnnotations.ID));
        assertTrue(OutputFilesGenerator.isAnnotatedWith(field, MappingAnnotations.PROPERTIES));
        assertTrue(OutputFilesGenerator.isAnnotatedWith(field, MappingAnnotations.TYPES));
        assertTrue(OutputFilesGenerator.isAnnotatedWith(field, MappingAnnotations.OBJECT_PROPERTY));
        assertTrue(OutputFilesGenerator.isAnnotatedWith(field, MappingAnnotations.ANNOTATION_PROPERTY));
        assertTrue(OutputFilesGenerator.isAnnotatedWith(field, MappingAnnotations.DATA_PROPERTY));
    }

    @Test
    void isAnnotatedWithFailTest() {
        Field field = new Field();
        assertFalse(OutputFilesGenerator.isAnnotatedWith(field, null));

        field.setAnnotatedWith(Collections.singletonList(MappingAnnotations.ID));
        assertFalse(OutputFilesGenerator.isAnnotatedWith(field, MappingAnnotations.PROPERTIES));
        assertFalse(OutputFilesGenerator.isAnnotatedWith(field, MappingAnnotations.TYPES));
        assertFalse(OutputFilesGenerator.isAnnotatedWith(field, MappingAnnotations.OBJECT_PROPERTY));
        assertFalse(OutputFilesGenerator.isAnnotatedWith(field, MappingAnnotations.ANNOTATION_PROPERTY));
        assertFalse(OutputFilesGenerator.isAnnotatedWith(field, MappingAnnotations.DATA_PROPERTY));
    }

    @Nested
    class BasicGeneratedOutputTest {

        private static String actualResult = "";

        @BeforeAll
        static void setUpBeforeAll() throws IOException {
            new OutputFilesGenerator(new OutputConfig("./src/test/java", false), false, null).generateOutputFiles(List.of(metamodelClass));

            actualResult = readFileAsString(new File(OUTPUT_FILE));
        }

        @AfterAll
        static void deleteSMClass() {
            File file = new File(OUTPUT_FILE);
            file.delete();
        }

        @Test
        void containsClassIri() {
            assertThat(actualResult, containsString("public static volatile IRI entityClassIRI;"));
        }

        @Test
        void containsIdentifier() {
            assertThat(actualResult, containsString("public static volatile Identifier<TestingClassOWL, Test> uri;"));
        }

        @Test
        void containsSingular() {
            assertThat(actualResult, containsString("public static volatile SingularAttribute<TestingClassOWL, Test> object;"));
        }

        @Test
        void containsTypedProperties() {
            assertThat(actualResult, containsString("public static volatile PropertiesSpecification<TestingClassOWL, Test> properties;"));
        }

        @Test
        void containsListAttribute() {
            assertThat(actualResult, containsString("public static volatile ListAttribute<TestingClassOWL, Test> dataList;"));
        }

        @Test
        void containsSetAttribute() {
            assertThat(actualResult, containsString("public static volatile SetAttribute<TestingClassOWL, Test> dataSet;"));
        }
    }

    @Nested
    class GeneratedOutputWithPropertyIrisTest {

        @AfterEach
        void tearDown() {
            File file = new File(OUTPUT_FILE);
            file.delete();
        }

        @Test
        void containsPropertyIris() throws IOException {
            new OutputFilesGenerator(new OutputConfig("./src/test/java", true), false, null).generateOutputFiles(List.of(metamodelClass));
            String actualResult = readFileAsString(new File(OUTPUT_FILE));

            assertThat(actualResult, containsString("public static volatile IRI objectPropertyIRI;"));
            assertThat(actualResult, containsString("public static volatile IRI dataListPropertyIRI;"));
            assertThat(actualResult, containsString("public static volatile IRI dataSetPropertyIRI;"));
        }
    }
}

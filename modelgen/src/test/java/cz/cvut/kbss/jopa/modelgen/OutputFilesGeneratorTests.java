package cz.cvut.kbss.jopa.modelgen;

import cz.cvut.kbss.jopa.modelgen.classmodel.Field;
import cz.cvut.kbss.jopa.modelgen.classmodel.MappingAnnotations;
import cz.cvut.kbss.jopa.modelgen.classmodel.MetamodelClass;
import cz.cvut.kbss.jopa.modelgen.classmodel.Type;
import cz.test.ex.TestingClassOWL;
import org.junit.jupiter.api.AfterAll;
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

    private static String actualResult = "";

    @BeforeAll
    static void createSMClass() throws IOException {
        MetamodelClass cl = new MetamodelClass();
        Class<TestingClassOWL> testingClass = TestingClassOWL.class;
        cl.setName(testingClass.getSimpleName());
        cl.setPckg(testingClass.getPackage().getName());
        cl.getImports().add(testingClass.getName());
        cl.setExtend("");

        Field field1 = new Field();
        field1.setAnnotatedWith(Collections.singletonList(MappingAnnotations.ID));
        field1.setName("uri");
        field1.setParentName("cz.test.ex.TestingClassOWL");
        Type type1 = new Type();
        type1.setTypeName("Test");
        field1.setType(type1);
        cl.addField(field1);

        Field field2 = new Field();
        field2.setAnnotatedWith(Collections.singletonList(MappingAnnotations.OBJECT_PROPERTY));
        field2.setName("object");
        field2.setParentName("cz.test.ex.TestingClassOWL");
        field2.setType(type1);
        cl.addField(field2);

        Field field3 = new Field();
        field3.setAnnotatedWith(Collections.singletonList(MappingAnnotations.TYPES));
        field3.setName("types");
        field3.setParentName("cz.test.ex.TestingClassOWL");
        field3.setType(type1);
        cl.addField(field3);

        Field field5 = new Field();
        field5.setAnnotatedWith(Collections.singletonList(MappingAnnotations.DATA_PROPERTY));
        field5.setName("dataList");
        field5.setParentName("cz.test.ex.TestingClassOWL");
        Type type5 = new Type();
        type5.setTypeName(List.class.getName());
        type5.setIsSimple(false);
        field5.setType(type5);
        type5.setTypes(Collections.singletonList(type1));
        field5.setType(type5);
        cl.addField(field5);

        Field field6 = new Field();
        field6.setAnnotatedWith(Collections.singletonList(MappingAnnotations.DATA_PROPERTY));
        field6.setName("dataSet");
        field6.setParentName("cz.test.ex.TestingClassOWL");
        Type type6 = new Type();
        type6.setTypeName(Set.class.getName());
        type6.setIsSimple(false);
        field6.setType(type6);
        type6.setTypes(Collections.singletonList(type1));
        field6.setType(type6);
        cl.addField(field6);

        Field field8 = new Field();
        field8.setAnnotatedWith(Collections.singletonList(MappingAnnotations.PROPERTIES));
        field8.setName("properties");
        field8.setParentName("cz.test.ex.TestingClassOWL");
        field8.setType(type6);
        type6.setTypes(Collections.singletonList(type1));
        field8.setType(type6);
        cl.addField(field8);

        new OutputFilesGenerator("./src/test/java", false, null).generateOutputFiles(List.of(cl));

        actualResult = readFileAsString(new File("./src/test/java/cz/test/ex/TestingClassOWL_.java"));
    }


    @AfterAll
    static void deleteSMClass() {
        File file = new File("./src/test/java/cz/test/ex/TestingClassOWL_.java");
        file.delete();
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
    class GenerateOutputFilesTest {

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
}

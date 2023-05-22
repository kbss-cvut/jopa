package cz.cvut.kbss.jopa.modelgen;

import cz.cvut.kbss.jopa.modelgen.classmodel.AnnotationEnum;
import cz.cvut.kbss.jopa.modelgen.classmodel.Field;
import cz.cvut.kbss.jopa.modelgen.classmodel.MetamodelClass;
import cz.cvut.kbss.jopa.modelgen.classmodel.Type;
import cz.test.ex.TestingClassOWL;
import org.junit.jupiter.api.*;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;


class OutputFilesGeneratorTests {

    static String actualResult = "";
    @BeforeAll
    static void createSMClass() throws FileNotFoundException {
        Map<String, MetamodelClass> map = new HashMap<>();
        MetamodelClass cl = new MetamodelClass();
        Class<TestingClassOWL> testingClass = TestingClassOWL.class;
        cl.setName(testingClass.getSimpleName());
        cl.setPckg(testingClass.getPackage().getName());
        cl.getImports().add(testingClass.getName());
        cl.setExtend("");

        Field field1 = new Field();
        field1.setAnnotatedWith(Arrays.asList(AnnotationEnum.ID));
        field1.setName("uri");
        field1.setParentName("cz.test.ex.TestingClassOWL");
        Type type1 = new Type();
        type1.setTypeName("Test");
        field1.setType(type1);
        cl.addField(field1);

        Field field2 = new Field();
        field2.setAnnotatedWith(Arrays.asList(AnnotationEnum.OBJECTPROPERTY));
        field2.setName("object");
        field2.setParentName("cz.test.ex.TestingClassOWL");
        field2.setType(type1);
        cl.addField(field2);

        Field field3 = new Field();
        field3.setAnnotatedWith(Arrays.asList(AnnotationEnum.TYPES));
        field3.setName("types");
        field3.setParentName("cz.test.ex.TestingClassOWL");
        field3.setType(type1);
        cl.addField(field3);

        Field field5 = new Field();
        field5.setAnnotatedWith(Arrays.asList(AnnotationEnum.DATAPROPERTY));
        field5.setName("dataList");
        field5.setParentName("cz.test.ex.TestingClassOWL");
        Type type5 = new Type();
        type5.setTypeName(List.class.getName());
        type5.setIsSimple(false);
        field5.setType(type5);
        type5.setTypes(Arrays.asList(type1));
        field5.setType(type5);
        cl.addField(field5);

        Field field6 = new Field();
        field6.setAnnotatedWith(Arrays.asList(AnnotationEnum.DATAPROPERTY));
        field6.setName("dataSet");
        field6.setParentName("cz.test.ex.TestingClassOWL");
        Type type6 = new Type();
        type6.setTypeName(Set.class.getName());
        type6.setIsSimple(false);
        field6.setType(type6);
        type6.setTypes(Arrays.asList(type1));
        field6.setType(type6);
        cl.addField(field6);

        Field field8 = new Field();
        field8.setAnnotatedWith(Arrays.asList(AnnotationEnum.PROPERTIES));
        field8.setName("properties");
        field8.setParentName("cz.test.ex.TestingClassOWL");
        field8.setType(type6);
        type6.setTypes(Arrays.asList(type1));
        field8.setType(type6);
        cl.addField(field8);

        map.put("cz.test.ex.TestingClassOWL", cl);
        OutputFilesGenerator.generateOutputFiles(map, "./src/test/java", null, false);

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

        field.setAnnotatedWith(AnnotationEnum.getAll());

        assertTrue(OutputFilesGenerator.isAnnotatedWith(field, AnnotationEnum.ID));
        assertTrue(OutputFilesGenerator.isAnnotatedWith(field, AnnotationEnum.PROPERTIES));
        assertTrue(OutputFilesGenerator.isAnnotatedWith(field, AnnotationEnum.TYPES));
        assertTrue(OutputFilesGenerator.isAnnotatedWith(field, AnnotationEnum.OBJECTPROPERTY));
        assertTrue(OutputFilesGenerator.isAnnotatedWith(field, AnnotationEnum.ANNOTATIONPROPERTY));
        assertTrue(OutputFilesGenerator.isAnnotatedWith(field, AnnotationEnum.DATAPROPERTY));
    }

    @Test
    void isAnnotatedWithFailTest() {
        Field field = new Field();
        assertFalse(OutputFilesGenerator.isAnnotatedWith(field, null));

        field.setAnnotatedWith(Arrays.asList(AnnotationEnum.ID));
        assertFalse(OutputFilesGenerator.isAnnotatedWith(field, AnnotationEnum.PROPERTIES));
        assertFalse(OutputFilesGenerator.isAnnotatedWith(field, AnnotationEnum.TYPES));
        assertFalse(OutputFilesGenerator.isAnnotatedWith(field, AnnotationEnum.OBJECTPROPERTY));
        assertFalse(OutputFilesGenerator.isAnnotatedWith(field, AnnotationEnum.ANNOTATIONPROPERTY));
        assertFalse(OutputFilesGenerator.isAnnotatedWith(field, AnnotationEnum.DATAPROPERTY));
    }

    @Nested
    class GenerateOutputFiles {

        @Test
        void containsIdentifier() {
            assertTrue(actualResult.contains("public static volatile Identifier<TestingClassOWL, Test> uri;"));
        }

        @Test
        void containsSingular() {
            assertTrue(actualResult.contains("public static volatile SingularAttribute<TestingClassOWL, Test> object;"));
        }

        @Test
        void containsTypedProperties() {
            assertTrue(actualResult.contains("public static volatile PropertiesSpecification<TestingClassOWL, Test> properties;"));
        }

        @Test
        void containsListAttribute() {
            assertTrue(actualResult.contains("public static volatile ListAttribute<TestingClassOWL, Test> dataList;"));
        }

        @Test
        void containsSetAttribute() {
            assertTrue(actualResult.contains("public static volatile SetAttribute<TestingClassOWL, Test> dataSet;"));
        }

    }


    static String readFileAsString(File file) throws FileNotFoundException {
        Scanner scanner = new Scanner(file);
        String fileContents = scanner.useDelimiter("\\Z").next();
        scanner.close();
        return fileContents;
    }
}

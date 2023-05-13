package cz.cvut.kbss.jopa.modelgen;

import cz.cvut.kbss.jopa.modelgen.classmodel.AnnotationEnum;
import cz.cvut.kbss.jopa.modelgen.classmodel.Field;
import cz.cvut.kbss.jopa.modelgen.classmodel.MetamodelClass;
import cz.cvut.kbss.jopa.modelgen.classmodel.Type;
import cz.test.ex.TestingClassOWL;
import org.junit.jupiter.api.*;

import java.io.File;
import java.io.IOException;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;


class OutputFilesGeneratorTests {

    static String actualResult = "";
    static final String expectedResult = readFileAsString(new File("./src/test/resources/TestingClass_.txt"));
    @BeforeAll
    static void createSMClass(){
        Map<String, MetamodelClass> map = new HashMap<>();
        MetamodelClass cl = new MetamodelClass();
        Class<TestingClassOWL> testingClass = TestingClassOWL.class;
        cl.setName(testingClass.getSimpleName());
        cl.setPckg(testingClass.getPackage().getName());
        cl.getImports().add(testingClass.getName());
        cl.setExtend("");

        Field field1 = new Field();
        field1.setAnnotatedWith(Arrays.asList(AnnotationEnum.ID));
        field1.setName("id");
        field1.setParentName("cz.test.ex.TestingClass");
        Type type1 = new Type();
        type1.setTypeName("Test");
        field1.setType(type1);
        cl.addField(field1);

        Field field2 = new Field();
        field2.setAnnotatedWith(Arrays.asList(AnnotationEnum.OBJECTPROPERTY));
        field2.setName("object");
        field2.setParentName("cz.test.ex.TestingClass");
        field2.setType(type1);
        cl.addField(field2);

        Field field3 = new Field();
        field3.setAnnotatedWith(Arrays.asList(AnnotationEnum.TYPES));
        field3.setName("types");
        field3.setParentName("cz.test.ex.TestingClass");
        field3.setType(type1);
        cl.addField(field3);

        Field field5 = new Field();
        field5.setAnnotatedWith(Arrays.asList(AnnotationEnum.DATAPROPERTY));
        field5.setName("data-list");
        field5.setParentName("cz.test.ex.TestingClass");
        Type type5 = new Type();
        type5.setTypeName(List.class.getName());
        type5.setIsSimple(false);
        field5.setType(type5);
        type5.setTypes(Arrays.asList(type1));
        field5.setType(type5);
        cl.addField(field5);

        Field field6 = new Field();
        field6.setAnnotatedWith(Arrays.asList(AnnotationEnum.DATAPROPERTY));
        field6.setName("data-set");
        field6.setParentName("cz.test.ex.TestingClass");
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
        field8.setParentName("cz.test.ex.TestingClass");
        field8.setType(type6);
        type6.setTypes(Arrays.asList(type1));
        field8.setType(type6);
        cl.addField(field8);

        map.put("cz.test.ex.TestingClass", cl);
        OutputFilesGenerator.generateOutputFiles(map, "./src/test/java", null, false);

         actualResult = readFileAsString(new File("./src/test/java/cz/test/ex/TestingClass_.java"));
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

        @AfterEach
        void deleteSMClass() {
            File file = new File("./src/test/java/cz/test/ex/TestingClass_.java");
            file.delete();
        }

        @Test
        void containsIdentifier() {
            assertTrue(actualResult.contains("public static volatile Identifier<TestingClass, URI> uri;"));
        }

        @Test
        void containsSingular() {
            assertTrue(actualResult.contains("public static volatile SingularAttribute<TestingClass, TestingClass> testingClass;"));
        }

        @Test
        void containsProperties() {
            assertTrue(actualResult.contains("public static volatile PropertiesSpecification<TestingClass, Map, URI, Object> propertie;"));
        }

        @Test
        void containsListAttribute() {
            assertTrue(actualResult.contains("public static volatile ListAttribute<TestingClass, String> listAttribute;"));
        }

        @Test
        void containsSetAttribute() {
            assertTrue(actualResult.contains("public static volatile SetAttribute<TestingClass, String> setAttribute;"));
        }


        String readFileAsString(File file) {
            try {
                Scanner scanner = new Scanner(file);
                String fileContents = scanner.useDelimiter("\\Z").next();
                scanner.close();
                return fileContents;
            } catch (IOException e) {
                System.err.println("Chyba cteni");
            }
            return "";
        }

    }


    static String readFileAsString(File file) {
        try {
            Scanner scanner = new Scanner(file);
            String fileContents = scanner.useDelimiter("\\Z").next();
            scanner.close();
            return fileContents;
        } catch (IOException e) {
            System.err.println("Chyba cteni");
        }
        return "";
    }
}

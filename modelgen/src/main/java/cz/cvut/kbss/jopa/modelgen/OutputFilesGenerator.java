package cz.cvut.kbss.jopa.modelgen;

import cz.cvut.kbss.jopa.modelgen.classmodel.MappingAnnotations;
import cz.cvut.kbss.jopa.modelgen.classmodel.Field;
import cz.cvut.kbss.jopa.modelgen.classmodel.MetamodelClass;
import cz.cvut.kbss.jopa.modelgen.classmodel.Type;

import javax.annotation.processing.Messager;
import javax.tools.Diagnostic;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * Class for generating output files
 */
public class OutputFilesGenerator {
    public static String finalTargetFolder = "";

    public static boolean debugOption;

    /**
     * Method for creating first part of final class file.
     * @param glass Object for which class want to be generated.
     * @return  returns File object.
     */
    public static File createClass(MetamodelClass glass) {
        StringBuilder fileName = new StringBuilder(finalTargetFolder);
        fileName.append("/");
        String pack = glass.getPckg();
        String extend = "";
        if (!glass.getExtend().isEmpty()) {
            extend = "extends ";
            if (glass.getExtend().contains(".")) {
                extend += glass.getExtend().substring(glass.getExtend().lastIndexOf(".") + 1) + "_ ";
            } else {
                extend += glass.getExtend();
            }
        }
        while (pack.contains(".")) {
            int index = pack.indexOf(".");
            fileName.append(pack, 0, index).append("/");
            pack = pack.substring(index + 1);
        }
        fileName.append(pack)
                .append("/")
                .append(glass.getName())
                .append("_.java");
        StringBuilder sbOut = new StringBuilder();

        if (!glass.getPckg().isEmpty()) {
            sbOut
                    .append("package ")
                    .append(glass.getPckg())
                    .append(";\n\n");
        }
        glass.getImports().forEach((imbort) -> {
            sbOut.append("import ")
                    .append(imbort)
                    .append(";\n");
        });
        if (!glass.getExtend().isEmpty()) {
            sbOut.append("import ")
                    .append(glass.getExtend())
                    .append("_;\n");
        }
        sbOut
                .append("\n@Generated(value = \"")
                .append("cz.cvut.kbss.jopa.modelgen.ModelGenProcessor\")")
                .append("\n@StaticMetamodel(")
                .append(glass.getName())
                .append(".class)\n")
                .append("public ")
                .append("abstract ")
                .append("class ")
                .append(glass.getName())
                .append("_ ")
                .append(extend)
                .append("{\n\n");
        try {
            File file = new File(fileName.toString());
            file.getParentFile().mkdirs();
            file.createNewFile();
            FileWriter out = new FileWriter(file);
            out.write(sbOut.toString());
            out.close();
            return file;
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Method for appending correct static metamodel fields to file.
     * @param glass object with fields
     * @param classFile file in which will be fields appended
     */
    public static void appendProperties(MetamodelClass glass, File classFile) {
        StringBuilder propertiesString = new StringBuilder();
        for (Field field : glass.getFields()) {
            propertiesString.append("\t public static volatile ");
            //@Id
            if (isAnnotatedWith(field, MappingAnnotations.ID)) {
                propertiesString
                        .append("Identifier<")
                        .append(field.getParentName().substring(field.getParentName().lastIndexOf(".") + 1))
                        .append(", ")
                        .append(field.getType().getTypeName().substring(field.getType().getTypeName().lastIndexOf(".") + 1));
                //@Types
            } else if (isAnnotatedWith(field, MappingAnnotations.TYPES)) {
                propertiesString
                        .append("TypesSpecification<")
                        .append(field.getParentName().substring(field.getParentName().lastIndexOf(".") + 1))
                        .append(", ");
                if (field.getType().getIsSimple()) {
                    propertiesString
                            .append(field.getType().getTypeName().substring(field.getType().getTypeName().lastIndexOf(".") + 1));
                } else {
                    propertiesString
                            .append(field.getType().getTypes().get(0).getTypeName().substring(field.getType().getTypes().get(0).getTypeName().lastIndexOf(".") + 1));
                }
                //@Properties
            } else if (isAnnotatedWith(field, MappingAnnotations.PROPERTIES)) {
                propertiesString
                        .append("PropertiesSpecification<")
                        .append(field.getParentName().substring(field.getParentName().lastIndexOf(".") + 1))
                        .append(", ");
                Type type = field.getType();
                if (!Objects.equals(type.getTypeName(), Map.class.getName())) {
                    propertiesString
                            .append(type.getTypes().get(0).getTypeName().substring(type.getTypes().get(0).getTypeName().lastIndexOf(".") + 1));
                } else {
                    propertiesString
                            .append("Map, ")
                            .append(type.getTypes().get(0).getTypeName().substring(type.getTypes().get(0).getTypeName().lastIndexOf(".") + 1))
                            .append(", ")
                            .append(type.getTypes().get(1).getTypes().get(0).getTypeName().substring(type.getTypes().get(1).getTypes().get(0).getTypeName().lastIndexOf(".") + 1));
                }
            } else if (isAnnotatedWith(field, MappingAnnotations.DATA_PROPERTY)
                    || isAnnotatedWith(field, MappingAnnotations.OBJECT_PROPERTY)
                    || isAnnotatedWith(field, MappingAnnotations.ANNOTATION_PROPERTY)) {
                Type type = field.getType();
                if (type.getIsSimple()) {
                    propertiesString
                            .append("SingularAttribute<")
                            .append(field.getParentName().substring(field.getParentName().lastIndexOf(".") + 1))
                            .append(", ")
                            .append(type.getTypeName().substring(type.getTypeName().lastIndexOf(".") + 1));
                } else {
                    if (type.getTypeName().equals(List.class.getName())) {
                        propertiesString
                                .append("ListAttribute<");
                    } else if (type.getTypeName().equals(Set.class.getName())) {
                        propertiesString
                                .append("SetAttribute<");
                    }
                    propertiesString
                            .append(field.getParentName().substring(field.getParentName().lastIndexOf(".") + 1))
                            .append(", ")
                            .append(type.getTypes().get(0).getTypeName().substring(type.getTypes().get(0).getTypeName().lastIndexOf(".") + 1));
                }
            }
            propertiesString
                    .append("> ")
                    .append(field.getName())
                    .append(";\n\n");
        }
        try {
            FileWriter fw = new FileWriter(classFile, true);
            fw.append(propertiesString);
            fw.close();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Method for closing the class file
     * @param outputFile class file wanted to be closed
     */
    public static void finishClass(File outputFile) {
        try {
            FileWriter fw = new FileWriter(outputFile, true);
            fw.append("}");
            fw.close();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Checking method whether Field has given annotation
     * @param field
     * @param mappingAnnotations
     * @return
     */
    public static boolean isAnnotatedWith(Field field, MappingAnnotations mappingAnnotations) {
        List<MappingAnnotations> annotations = field.getAnnotatedWith();
        if (annotations.isEmpty()) return false;
        for (MappingAnnotations an : annotations) {
            if (an.equals(mappingAnnotations)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Class which will generate all given MetamodelClass objects. It works with every method in OutputFilesGenerator class.
     * @param classes   MetamodelClass objects wanted to be generated.
     * @param outputDirectory Output directory for metamodel
     * @param messager messager for console output
     * @param debugOption controller of console output.
     */

    public static void generateOutputFiles(Map<String, MetamodelClass> classes, String outputDirectory, Messager messager, boolean debugOption) {
        finalTargetFolder = outputDirectory;
        OutputFilesGenerator.debugOption = debugOption;
        for (Map.Entry<String, MetamodelClass> entry : classes.entrySet()) {
            MetamodelClass glass = entry.getValue();
            File outputFile = createClass(glass);
            appendProperties(glass, outputFile);
            finishClass(outputFile);
            if (debugOption)
                messager.printMessage(Diagnostic.Kind.NOTE, "\t - File " + outputFile.getName() + " created.");
        }
    }
}

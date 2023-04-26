package cz.cvut.kbss.jopa.modelgen;

import cz.cvut.kbss.jopa.modelgen.classmodel.Field;
import cz.cvut.kbss.jopa.modelgen.classmodel.MetamodelClass;
import cz.cvut.kbss.jopa.modelgen.classmodel.Type;

import javax.annotation.processing.Messager;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

public class OutputFilesGenerator {
    public static String finalTargetFolder = "";

    public static File createClass(MetamodelClass glass) {
        StringBuilder fileName = new StringBuilder(finalTargetFolder);
        fileName.append("/");
        String pack = glass.getPckg();
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
        sbOut
                .append("\n@Generated(value = \"")
                .append("cz.cvut.kbss.jopa.modelgen.ModelGenProcessor\")")
                .append("\n@StaticMetamodel(")
                .append(glass.getName())
                .append(".class)\n")
                .append("public class ")
                .append(glass.getName())
                .append("_ {\n\n");
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

    public static void appendProperties(MetamodelClass glass, File classFile) {
        StringBuilder propertiesString = new StringBuilder();
        for (Field field : glass.getProperties()) {
            propertiesString.append("\t public static volatile ");
            //@Id
            if (isAnnotatedWith(field, AnnotationEnum.ID)) {
                propertiesString
                        .append("Identifier<")
                        .append(field.getParentName().substring(field.getParentName().lastIndexOf(".") + 1))
                        .append(", ")
                        .append(field.getType().getTypeName().substring(field.getType().getTypeName().lastIndexOf(".") + 1));
                //@Types
            } else if (isAnnotatedWith(field, AnnotationEnum.TYPES)) {
                propertiesString
                        .append("TypesSpecification<")
                        .append(field.getParentName().substring(field.getParentName().lastIndexOf(".") + 1))
                        .append(", ");
                if (field.getType().getIsSimple()) {
                    //TODO: raw types? napriklad Set<> Object?? idk
                    propertiesString
                            .append(field.getType().getTypeName().substring(field.getType().getTypeName().lastIndexOf(".") + 1));
                } else {
                    propertiesString
                            .append(field.getType().getTypes().get(0).getTypeName().substring(field.getType().getTypes().get(0).getTypeName().lastIndexOf(".") + 1));
                }
                //@Properties
            } else if (isAnnotatedWith(field, AnnotationEnum.PROPERTIES)) {
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
            } else if (isAnnotatedWith(field, AnnotationEnum.DATAPROPERTY)
                    || isAnnotatedWith(field, AnnotationEnum.OBJECTPROPERTY)
                    || isAnnotatedWith(field, AnnotationEnum.ANNOTATIONPROPERTY)) {
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
                    .append(";\n");
        }
        try {
            FileWriter fw = new FileWriter(classFile, true);
            fw.append(propertiesString);
            fw.close();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static void finishClass(File outputFile) {
        try {
            FileWriter fw = new FileWriter(outputFile, true);
            fw.append("}");
            fw.close();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static boolean isAnnotatedWith(Field field, AnnotationEnum annotationEnum) {
        List<AnnotationEnum> annotations = field.getAnnotatedWith();
        if (annotations.isEmpty()) return false;
        for (AnnotationEnum an : annotations) {
            if (an.equals(annotationEnum)) {
                return true;
            }
        }
        return false;
    }

    public static void generateOutputFiles(Map<String, MetamodelClass> classes, String outputDirectory, Messager messager) {
        finalTargetFolder = outputDirectory;
        for (Map.Entry<String, MetamodelClass> entry : classes.entrySet()) {
            MetamodelClass glass = entry.getValue();
            File outputFile = createClass(glass);
            appendProperties(glass, outputFile);
            finishClass(outputFile);
            //messager.printMessage(Diagnostic.Kind.NOTE, "\t - File " + outputFile.getName() + " created.");
        }
    }
}

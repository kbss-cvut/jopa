package cz.cvut.kbss.jopa.modelgen;

import javax.lang.model.element.AnnotationMirror;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

public class OutputFilesGenerator {
    private static final String DEFAULT_TARGET_FOLDER = "./target/generated-sources/static-metamodel/";
    public static String customTargetFolder = "";

    public static File createClass(MetamodelClass glass) {
        StringBuilder fileName = new StringBuilder(DEFAULT_TARGET_FOLDER);
        String pack = glass.getPaggage();
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

        if (!glass.getPaggage().isEmpty()) {
            sbOut
                    .append("package ")
                    .append(glass.getPaggage())
                    .append(";\n\n");
        }
        glass.getImports().forEach((imbort) -> {
            sbOut.append("import ")
                    .append(imbort)
                    .append(";\n");
        });
        sbOut
                .append("\n@StaticMetamodel(")
                .append(glass.getName())
                .append(".class)\n")
                .append("public class ")
                .append(glass.getName())
                .append("_ {\n");

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
        for (Property property : glass.getProperties()) {
            propertiesString.append("\n\t public static volatile ");
            //@Id
            if (isAnnotatedWith(property, AnnotationEnum.ID)) {
                propertiesString
                        .append("Identifier<")
                        .append(property.getParentName().substring(property.getParentName().lastIndexOf(".") + 1))
                        .append(", ")
                        .append(property.getType().getTypeName().substring(property.getType().getTypeName().lastIndexOf(".") + 1));
                //@Types
            } else if (isAnnotatedWith(property, AnnotationEnum.TYPES)) {
                propertiesString
                        .append("TypesSpecification<")
                        .append(property.getParentName().substring(property.getParentName().lastIndexOf(".") + 1))
                        .append(", ");
                if (property.getType().getSimple()) {
                    //TODO: raw types? napriklad Set<> Object?? idk
                    propertiesString
                            .append(property.getType().getTypeName().substring(property.getType().getTypeName().lastIndexOf(".") + 1));
                } else {
                    propertiesString
                            .append(property.getType().getTypes().get(0).getTypeName().substring(property.getType().getTypes().get(0).getTypeName().lastIndexOf(".") + 1));
                }
                //@Properties
            } else if (isAnnotatedWith(property, AnnotationEnum.PROPERTIES)) {
                propertiesString
                        .append("PropertiesSpecification<")
                        .append(property.getParentName().substring(property.getParentName().lastIndexOf(".") + 1))
                        .append(", ");
                Type type = property.getType();
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
            } else if (isAnnotatedWith(property, AnnotationEnum.DATAPROPERTY)
                    || isAnnotatedWith(property, AnnotationEnum.OBJECTPROPERTY)
                    || isAnnotatedWith(property, AnnotationEnum.ANNOTATIONPROPERTY)) {
                Type type = property.getType();
                if (type.getSimple()) {
                    propertiesString
                            .append("SingularAttribute<")
                            .append(property.getParentName().substring(property.getParentName().lastIndexOf(".") + 1))
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
                            .append(property.getParentName().substring(property.getParentName().lastIndexOf(".") + 1))
                            .append(", ")
                            .append(type.getTypes().get(0).getTypeName().substring(type.getTypes().get(0).getTypeName().lastIndexOf(".") + 1));
                }
            }
            propertiesString
                    .append("> ")
                    .append(property.getName())
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

    public static boolean isAnnotatedWith(Property property, AnnotationEnum annotationEnum) {
        List<? extends AnnotationMirror> annotations = property.getAnnotatedWith();
        if (annotations.isEmpty()) return false;
        for (AnnotationMirror an : annotations) {
            if (an.getAnnotationType().toString().contains(annotationEnum.getAnnotation())) {
                return true;
            }
        }
        return false;
    }

    public static void generateOutputFiles(Map<String, MetamodelClass> classes) {
        for (Map.Entry<String, MetamodelClass> entry : classes.entrySet()) {
            MetamodelClass glass = entry.getValue();
            File outputFile = createClass(glass);
            appendProperties(glass, outputFile);
            finishClass(outputFile);
        }
    }
}

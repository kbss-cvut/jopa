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
import cz.cvut.kbss.jopa.modelgen.exception.ModelGenException;

import javax.annotation.processing.Messager;
import javax.tools.Diagnostic;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

/**
 * Class for generating output files
 */
public class OutputFilesGenerator {

    private static final String INDENT = "    ";

    private final OutputConfig outputConfig;
    private final boolean debugMode;

    private final Messager messager;

    public OutputFilesGenerator(OutputConfig outputConfig, boolean debugMode, Messager messager) {
        this.outputConfig = outputConfig;
        this.debugMode = debugMode;
        this.messager = messager;
    }

    /**
     * Generates files containing Java classes representing the specified static metamodel objects.
     *
     * @param classes Metamodel objects
     */
    public void generateOutputFiles(Collection<MetamodelClass> classes) {
        for (MetamodelClass cls : classes) {
            generateClassFile(cls);
        }
    }

    private void debug(String msg) {
        if (debugMode) {
            messager.printMessage(Diagnostic.Kind.NOTE, msg);
        }
    }

    private void generateClassFile(MetamodelClass cls) {
        final File targetFile = createTargetFile(cls);
        final StringBuilder content = new StringBuilder(generateClassPreamble(cls));
        generateClassIriField(cls).ifPresent(content::append);
        generatePropertyIris(cls).ifPresent(content::append);
        content.append(generateAttributes(cls));
        content.append(generateClassSuffix());
        try {
            Files.writeString(targetFile.toPath(), content.toString(), StandardOpenOption.TRUNCATE_EXISTING);
        } catch (IOException e) {
            throw new ModelGenException("Unable to output content to target file '" + targetFile + "'!", e);
        }
        debug("\t - File '" + targetFile.getName() + "' created.");
    }

    private File createTargetFile(MetamodelClass cls) {
        final StringBuilder fileName = new StringBuilder(outputConfig.targetDir());
        fileName.append("/");
        String pack = cls.getPckg();
        while (pack.contains(".")) {
            int index = pack.indexOf(".");
            fileName.append(pack, 0, index).append("/");
            pack = pack.substring(index + 1);
        }
        fileName.append(pack)
                .append("/")
                .append(cls.getName())
                .append("_.java");
        try {
            File file = new File(fileName.toString());
            Files.createDirectories(file.getParentFile().toPath());
            if (!file.exists()) {
                boolean result = file.createNewFile();
                if (!result) {
                    throw new ModelGenException("Unable to create target file '" + file + "'!");
                }
            }
            return file;
        } catch (IOException e) {
            throw new ModelGenException("Unable to create target file '" + fileName + "'!", e);
        }
    }

    public String generateClassPreamble(MetamodelClass cls) {
        String extend = "";
        if (!cls.getExtend().isEmpty()) {
            extend = "extends ";
            if (cls.getExtend().contains(".")) {
                extend += cls.getExtend().substring(cls.getExtend().lastIndexOf(".") + 1) + "_ ";
            } else {
                extend += cls.getExtend();
            }
        }
        StringBuilder sbOut = new StringBuilder();

        if (!cls.getPckg().isEmpty()) {
            sbOut.append("package ")
                 .append(cls.getPckg())
                 .append(";\n\n");
        }
        cls.getImports().forEach(imp -> sbOut.append("import ")
                                             .append(imp)
                                             .append(";\n"));
        if (!cls.getExtend().isEmpty()) {
            sbOut.append("import ")
                 .append(cls.getExtend())
                 .append("_;\n");
        }
        sbOut.append("\n@Generated(value = \"")
             .append("cz.cvut.kbss.jopa.modelgen.ModelGenProcessor\")\n")
             .append("@StaticMetamodel(")
             .append(cls.getName())
             .append(".class)\n")
             .append("public abstract class ")
             .append(cls.getName())
             .append("_ ")
             .append(extend)
             .append("{\n\n");
        return sbOut.toString();
    }

    private static Optional<String> generateClassIriField(MetamodelClass cls) {
        if (cls.isEntityClass()) {
            return Optional.of(INDENT + "public static volatile IRI entityClassIRI;\n\n");
        }
        return Optional.empty();
    }

    private Optional<String> generatePropertyIris(MetamodelClass cls) {
        if (!outputConfig.outputPropertyIris()) {
            return Optional.empty();
        }
        final StringBuilder sb = new StringBuilder();
        for (Field field : cls.getFields()) {
            if (isAnnotatedWith(field, MappingAnnotations.DATA_PROPERTY)
                    || isAnnotatedWith(field, MappingAnnotations.OBJECT_PROPERTY)
                    || isAnnotatedWith(field, MappingAnnotations.ANNOTATION_PROPERTY)) {
                sb.append(INDENT + "public static volatile IRI ");
                sb.append(field.getName());
                sb.append("PropertyIRI;\n");
            }
        }
        sb.append('\n');
        return Optional.of(sb.toString());
    }

    private static String generateAttributes(MetamodelClass cls) {
        StringBuilder attributes = new StringBuilder();
        for (Field field : cls.getFields()) {
            final String declaringClass = field.getParentName().substring(field.getParentName().lastIndexOf('.') + 1);
            attributes.append(INDENT + "public static volatile ");
            //@Id
            if (isAnnotatedWith(field, MappingAnnotations.ID)) {
                attributes.append("Identifier<")
                          .append(declaringClass)
                          .append(", ")
                          .append(field.getType().getTypeName()
                                       .substring(field.getType().getTypeName().lastIndexOf(".") + 1));
                //@Types
            } else if (isAnnotatedWith(field, MappingAnnotations.TYPES)) {
                attributes.append("TypesSpecification<")
                          .append(declaringClass)
                          .append(", ");
                if (field.getType().getIsSimple()) {
                    attributes.append(field.getType().getSimpleName());
                } else {
                    attributes.append(field.getType().getTypes().get(0).getSimpleName());
                }
                //@Properties
            } else if (isAnnotatedWith(field, MappingAnnotations.PROPERTIES)) {
                attributes.append("PropertiesSpecification<")
                          .append(declaringClass)
                          .append(", ");
                Type type = field.getType();
                if (!Objects.equals(type.getTypeName(), Map.class.getName())) {
                    attributes
                            .append(type.getTypes().get(0).getTypeName()
                                        .substring(type.getTypes().get(0).getTypeName().lastIndexOf(".") + 1));
                } else {
                    attributes.append("Map, ")
                              .append(type.getTypes().get(0).getSimpleName())
                              .append(", ")
                              .append(type.getTypes().get(1).getTypes().get(0).getSimpleName());
                }
            } else if (isAnnotatedWith(field, MappingAnnotations.DATA_PROPERTY)
                    || isAnnotatedWith(field, MappingAnnotations.OBJECT_PROPERTY)
                    || isAnnotatedWith(field, MappingAnnotations.ANNOTATION_PROPERTY)) {
                Type type = field.getType();
                if (type.getIsSimple()) {
                    attributes.append("SingularAttribute<")
                              .append(declaringClass)
                              .append(", ")
                              .append(type.getSimpleName());
                } else {
                    if (type.getTypeName().equals(List.class.getName())) {
                        attributes.append("ListAttribute<");
                    } else if (type.getTypeName().equals(Set.class.getName())) {
                        attributes.append("SetAttribute<");
                    }
                    attributes.append(declaringClass)
                              .append(", ")
                              .append(type.getTypes().get(0).getSimpleName());
                }
            }
            attributes
                    .append("> ")
                    .append(field.getName())
                    .append(";\n");
        }
        return attributes.toString();
    }

    private static String generateClassSuffix() {
        return "}";
    }

    /**
     * Checking method whether Field has at least one of the given annotations.
     *
     * @param field              Field to check
     * @param mappingAnnotations Annotations for which to check
     * @return {@code true} if the field is annotated with at least one of the mapping annotations, {@code false}
     * otherwise
     */
    static boolean isAnnotatedWith(Field field, MappingAnnotations mappingAnnotations) {
        List<MappingAnnotations> annotations = field.getAnnotatedWith();
        if (annotations.isEmpty()) {
            return false;
        }
        for (MappingAnnotations an : annotations) {
            if (an.equals(mappingAnnotations)) {
                return true;
            }
        }
        return false;
    }
}

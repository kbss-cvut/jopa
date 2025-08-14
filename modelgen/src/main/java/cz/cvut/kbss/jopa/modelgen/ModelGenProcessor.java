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

import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.Messager;
import javax.annotation.processing.ProcessingEnvironment;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.annotation.processing.SupportedOptions;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.tools.Diagnostic;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Annotation processor that finds JOPA entities and mapped superclasses and generates a static metamodel based on
 * them.
 */
@SupportedAnnotationTypes({"cz.cvut.kbss.jopa.model.annotations.OWLClass",
        "cz.cvut.kbss.jopa.model.annotations.MappedSuperclass"})
@SupportedOptions({
        ModelGenProcessor.OUTPUT_DIRECTORY_PARAM,
        ModelGenProcessor.SOURCE_PACKAGE_PARAM,
        ModelGenProcessor.DEBUG_PARAM
})
public class ModelGenProcessor extends AbstractProcessor {
    public static final String OUTPUT_DIRECTORY_PARAM = "outputDirectory";
    public static final String SOURCE_PACKAGE_PARAM = "sourcePackage";
    public static final String DEBUG_PARAM = "debugOption";
    Messager messager;

    private Map<String, MetamodelClass> classes;

    private String sourcePackage;
    private String outputDirectory;
    private boolean debugOption;

    @Override
    public void init(ProcessingEnvironment env) {
        super.init(env);
        this.messager = env.getMessager();
        messager.printMessage(Diagnostic.Kind.NOTE, "Initializing ModelGenProcessor.");
        this.classes = new HashMap<>();
        sourcePackage = env.getOptions().get(SOURCE_PACKAGE_PARAM);
        outputDirectory = env.getOptions().get(OUTPUT_DIRECTORY_PARAM);
        debugOption = Boolean.parseBoolean(env.getOptions().get(DEBUG_PARAM));
    }

    @Override
    public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
        for (TypeElement te : annotations) {
            for (Element elParent : roundEnv.getElementsAnnotatedWith(te)) {
                if (!isAnnotatedWithNonEntity(elParent) && (sourcePackage == null || elParent.asType().toString()
                                                                                             .contains(sourcePackage))) {
                    MetamodelClass parentClass = new MetamodelClass(elParent);
                    if (isAnnotationWithOwlClass(elParent)) {
                        parentClass.makeEntityClass();
                    }

                    if (debugOption) {
                        messager.printMessage(Diagnostic.Kind.NOTE,
                                "\t - Started processing class '" + parentClass.getName() + "'");
                    }
                    List<? extends Element> properties = elParent.getEnclosedElements();
                    for (Element elProperty : properties) {
                        if (isPropertyPersistent(elProperty)) {
                            Field field = new Field(elProperty, elParent);
                            if (debugOption) {
                                messager.printMessage(Diagnostic.Kind.NOTE,
                                        "\t\t - Processing field '" + field.getName() + "'");
                            }
                            parentClass.addField(field);
                        }
                    }
                    classes.put(elParent.toString(), parentClass);
                    if (debugOption) {
                        messager.printMessage(Diagnostic.Kind.NOTE,
                                "\t - Finished processing class '" + parentClass.getName() + "'");
                    }
                }
            }
        }
        if (debugOption) {
            messager.printMessage(Diagnostic.Kind.NOTE, "Generating output files.");
        }
        final OutputFilesGenerator outputGenerator = new OutputFilesGenerator(outputDirectory, debugOption, messager);
        outputGenerator.generateOutputFiles(classes.values());
        return true;
    }

    private static boolean isPropertyPersistent(Element param) {
        boolean containsWanted = false;
        List<? extends AnnotationMirror> paramAnnotations = param.getAnnotationMirrors();
        if (!paramAnnotations.isEmpty()) {
            for (AnnotationMirror paramAnnotation : paramAnnotations) {
                if (paramAnnotation.toString().contains("cz.cvut.kbss.jopa.model.annotations.Transient")) {
                    return false;
                }
                for (MappingAnnotations anEnum : MappingAnnotations.values()) {
                    if (paramAnnotation.toString().contains(anEnum.getAnnotation())) {
                        containsWanted = true;
                        break;
                    }
                }

            }
        }
        return containsWanted;
    }

    private static boolean isAnnotatedWithNonEntity(Element element) {
        return isAnnotatedWith(element, "cz.cvut.kbss.jopa.model.annotations.util.NonEntity");
    }

    private static boolean isAnnotatedWith(Element element, String annotationCls) {
        TypeElement typeElement = (TypeElement) element;
        List<? extends AnnotationMirror> annotations = typeElement.getAnnotationMirrors();
        for (AnnotationMirror annotation : annotations) {
            if (annotationCls.equals(annotation.getAnnotationType().toString())) {
                return true;
            }
        }
        return false;
    }

    private static boolean isAnnotationWithOwlClass(Element element) {
        return isAnnotatedWith(element, "cz.cvut.kbss.jopa.model.annotations.OWLClass");
    }

    @Override
    public SourceVersion getSupportedSourceVersion() {
        return SourceVersion.latestSupported();
    }
}

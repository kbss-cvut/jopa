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
import java.util.Optional;
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
        ModelGenProcessor.DEBUG_PARAM,
        ModelGenProcessor.OUTPUT_PROPERTY_IRIS_PARAM,
        ModelGenProcessor.OUTPUT_IRI_AS_STRING_PARAM,
        ModelGenProcessor.INITIALIZE_IRI_FIELDS_PARAM
})
public class ModelGenProcessor extends AbstractProcessor {
    public static final String OUTPUT_DIRECTORY_PARAM = "outputDirectory";
    public static final String SOURCE_PACKAGE_PARAM = "sourcePackage";
    public static final String DEBUG_PARAM = "debugOption";
    public static final String OUTPUT_PROPERTY_IRIS_PARAM = "outputPropertyIris";
    public static final String OUTPUT_IRI_AS_STRING_PARAM = "outputIriAsString";
    public static final String INITIALIZE_IRI_FIELDS_PARAM = "initializeIris";

    Messager messager;

    private final Map<String, MetamodelClass> classes = new HashMap<>();

    private String sourcePackage;
    private String outputDirectory;
    private boolean debugOption;
    private boolean outputPropertyIris;
    private boolean outputIriAsString;
    private boolean initializeIris;

    @Override
    public void init(ProcessingEnvironment env) {
        super.init(env);
        this.messager = env.getMessager();
        messager.printMessage(Diagnostic.Kind.NOTE, "Initializing ModelGenProcessor.");
        this.sourcePackage = env.getOptions().get(SOURCE_PACKAGE_PARAM);
        this.outputDirectory = env.getOptions().get(OUTPUT_DIRECTORY_PARAM);
        this.debugOption = Boolean.parseBoolean(env.getOptions().get(DEBUG_PARAM));
        this.outputPropertyIris = Boolean.parseBoolean(env.getOptions().get(OUTPUT_PROPERTY_IRIS_PARAM));
        this.outputIriAsString = Boolean.parseBoolean(env.getOptions().get(OUTPUT_IRI_AS_STRING_PARAM));
        this.initializeIris = Boolean.parseBoolean(env.getOptions().get(INITIALIZE_IRI_FIELDS_PARAM));
    }

    @Override
    public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
        for (TypeElement te : annotations) {
            for (Element clsElement : roundEnv.getElementsAnnotatedWith(te)) {
                if (!isAnnotatedWithNonEntity(clsElement) && (sourcePackage == null || clsElement.asType().toString()
                                                                                             .contains(sourcePackage))) {
                    MetamodelClass modelClass = new MetamodelClass(clsElement);
                    debug("\t - Started processing class '" + modelClass.getName() + "'");

                    final Optional<AnnotationMirror> owlClassAnn = resolveOwlClassAnnotation(clsElement);
                    owlClassAnn.ifPresent(modelClass::makeEntityClass);

                    List<? extends Element> properties = clsElement.getEnclosedElements();
                    for (Element elProperty : properties) {
                        if (isPropertyPersistent(elProperty)) {
                            Field field = new Field(elProperty, clsElement);
                            debug("\t\t - Processing field '" + field.getName() + "'");
                            modelClass.addField(field);
                        }
                    }
                    classes.put(clsElement.toString(), modelClass);
                    debug("\t - Finished processing class '" + modelClass.getName() + "'");
                }
            }
        }
        debug("Generating output files.");
        final OutputConfig outputConfig = new OutputConfig(outputDirectory, outputPropertyIris, outputIriAsString, initializeIris);
        final OutputFilesGenerator outputGenerator = new OutputFilesGenerator(outputConfig, debugOption, messager);
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
        return resolveAnnotation(element, "cz.cvut.kbss.jopa.model.annotations.util.NonEntity").isPresent();
    }

    private static Optional<AnnotationMirror> resolveAnnotation(Element element, String annotationCls) {
        TypeElement typeElement = (TypeElement) element;
        List<? extends AnnotationMirror> annotations = typeElement.getAnnotationMirrors();
        for (AnnotationMirror annotation : annotations) {
            if (annotationCls.equals(annotation.getAnnotationType().toString())) {
                return Optional.of(annotation);
            }
        }
        return Optional.empty();
    }

    private static Optional<AnnotationMirror> resolveOwlClassAnnotation(Element element) {
        return resolveAnnotation(element, "cz.cvut.kbss.jopa.model.annotations.OWLClass");
    }

    private void debug(String message) {
        if (debugOption) {
            messager.printMessage(Diagnostic.Kind.NOTE, message);
        }
    }

    @Override
    public SourceVersion getSupportedSourceVersion() {
        return SourceVersion.latestSupported();
    }
}

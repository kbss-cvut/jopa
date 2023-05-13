package cz.cvut.kbss.jopa.modelgen;


import cz.cvut.kbss.jopa.modelgen.classmodel.AnnotationEnum;
import cz.cvut.kbss.jopa.modelgen.classmodel.Field;
import cz.cvut.kbss.jopa.modelgen.classmodel.MetamodelClass;

import javax.annotation.processing.*;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.lang.model.util.Elements;
import javax.tools.Diagnostic;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

@SupportedAnnotationTypes({"cz.cvut.kbss.jopa.model.annotations.OWLClass", "cz.cvut.kbss.jopa.model.annotations.MappedSuperclass"})
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
    private Elements elementUtils;

    private Map<String, MetamodelClass> classes;

    private ProcessingEnvironment env;

    private String sourcePackage;
    private String outputDirectory;
    private boolean debugOption;

    @Override
    public void init(ProcessingEnvironment env) {
        super.init(env);
        this.env = env;
        messager = env.getMessager();
        messager.printMessage(Diagnostic.Kind.NOTE, "Inicializing ModelGenProcessor.");
        this.elementUtils = env.getElementUtils();
        classes = new HashMap<>();
        sourcePackage = env.getOptions().get(SOURCE_PACKAGE_PARAM);
        outputDirectory = env.getOptions().get(OUTPUT_DIRECTORY_PARAM);
        debugOption = Boolean.parseBoolean(env.getOptions().get(DEBUG_PARAM));
    }

    @Override
    public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
        for (TypeElement te : annotations) {
            for (Element elParent : roundEnv.getElementsAnnotatedWith(te)) {
                if (!isAnnotatedWithNonEntity(elParent)) {
                    if (sourcePackage == null || elParent.asType().toString().contains(sourcePackage)) {
                        MetamodelClass parentClass = new MetamodelClass(elParent);

                        if (debugOption)
                            messager.printMessage(Diagnostic.Kind.NOTE, "\t - Started processing class " + parentClass.getName());
                        List<? extends Element> properties = elParent.getEnclosedElements();
                        for (Element elProperty : properties) {
                            if (propertyIsWanted(elProperty)) {
                                Field field = new Field(elProperty, elParent);
                                if (debugOption)
                                    messager.printMessage(Diagnostic.Kind.NOTE, "\t\t - Processing field " + field.getName());
                                parentClass.addField(field);
                            }
                        }
                        classes.put(elParent.toString(), parentClass);
                        if (debugOption)
                            messager.printMessage(Diagnostic.Kind.NOTE, "\t - Finished processing " + parentClass.getName());
                    }
                }
            }
        }
        if (debugOption) messager.printMessage(Diagnostic.Kind.NOTE, "Starting to generate output files:");
        OutputFilesGenerator.generateOutputFiles(classes, outputDirectory, messager, debugOption);
        return true;
    }

    private boolean propertyIsWanted(Element param) {
        boolean containsWanted = false;
        List<? extends AnnotationMirror> paramAnnotations = param.getAnnotationMirrors();
        if (!paramAnnotations.isEmpty()) {
            for (AnnotationMirror paramAnnotation : paramAnnotations) {
                if (paramAnnotation.toString().contains("cz.cvut.kbss.jopa.model.annotations.Transient")) {
                    return false;
                }
                for (AnnotationEnum anEnum : AnnotationEnum.values()) {
                    if (paramAnnotation.toString().contains(anEnum.getAnnotation())) {
                        containsWanted = true;
                        break;
                    }
                }

            }
        }
        return containsWanted;
    }

    @Override
    public SourceVersion getSupportedSourceVersion() {
        return SourceVersion.latestSupported();
    }

    public ProcessingEnvironment getEnv() {
        return env;
    }

    public boolean isAnnotatedWithNonEntity(Element element) {
        TypeElement typeElement = (TypeElement) element;
        List<? extends AnnotationMirror> annotations = typeElement.getAnnotationMirrors();
        for (AnnotationMirror annotation : annotations) {
            if (annotation.getAnnotationType().toString().equals("cz.cvut.kbss.jopa.model.annotations.util.NonEntity")) {
                return true;
            }
        }
        return false;
    }
}
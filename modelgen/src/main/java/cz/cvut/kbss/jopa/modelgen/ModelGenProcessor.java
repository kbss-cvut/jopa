package cz.cvut.kbss.jopa.modelgen;


import cz.cvut.kbss.jopa.modelgen.classmodel.Field;
import cz.cvut.kbss.jopa.modelgen.classmodel.MetamodelClass;

import javax.annotation.processing.*;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.lang.model.util.Elements;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

@SupportedAnnotationTypes({"cz.cvut.kbss.jopa.model.annotations.OWLClass", "cz.cvut.kbss.jopa.model.annotations.MappedSuperclass"})
public class ModelGenProcessor extends AbstractProcessor {
    Messager messager;
    private Elements elementUtils;

    private Map<String, MetamodelClass> classes;

    private ProcessingEnvironment env;

    @Override
    public void init(ProcessingEnvironment env) {
        messager = env.getMessager();
        super.init(env);
        this.env = env;
        this.elementUtils = env.getElementUtils();
        classes = new HashMap<>();
    }

    @Override
    public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
        for (TypeElement te : annotations) {
            for (Element elParent : roundEnv.getElementsAnnotatedWith(te)) {
                MetamodelClass parentClass = new MetamodelClass(elParent);
                List<? extends Element> properties = elParent.getEnclosedElements();
                for (Element elProperty : properties) {
                    if (propertyIsWanted(elProperty)) {
                        Field field = new Field(elProperty, elParent);
                        parentClass.addProperty(field);
                    }
                }
                classes.put(elParent.toString(), parentClass);
            }
        }
        OutputFilesGenerator.generateOutputFiles(classes, null);
        return true;
    }

    private boolean propertyIsWanted(Element param) {
        List<? extends AnnotationMirror> paramAnnotations = param.getAnnotationMirrors();
        if (!paramAnnotations.isEmpty()) {
            for (AnnotationMirror paramAnnotation : paramAnnotations) {
                for (AnnotationEnum anEnum : AnnotationEnum.values()) {
                    if (paramAnnotation.toString().contains(anEnum.getAnnotation())) return true;
                }
            }
        }
        return false;
    }

    @Override
    public SourceVersion getSupportedSourceVersion() {
        return SourceVersion.latestSupported();
    }

    public ProcessingEnvironment getEnv() {
        return env;
    }
}
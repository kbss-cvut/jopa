package cz.cvut.kbss.jopa.modelgen;


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
public class AnnotationProcessor extends AbstractProcessor {
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
                        Property property = new Property(elProperty, elParent);
                        parentClass.addProperty(property);
                    }
                }
                classes.put(elParent.toString(), parentClass);
            }
        }
        Parser.generateOutputFiles(classes);
        return true;
    }

    private boolean propertyIsWanted(Element param) {
        List<? extends AnnotationMirror> paramAnnotations = param.getAnnotationMirrors();
        if (!paramAnnotations.isEmpty()) {
            for (AnnotationMirror paramAnnotation : paramAnnotations) {
                if (paramAnnotation.toString().contains("@cz.cvut.kbss.jopa.model.annotations.Id") ||
                        paramAnnotation.toString().contains("@cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty") ||
                        paramAnnotation.toString().contains("@cz.cvut.kbss.jopa.model.annotations.OWLDataProperty") ||
                        paramAnnotation.toString().contains("@cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty") ||
                        paramAnnotation.toString().contains("@cz.cvut.kbss.jopa.model.annotations.Types") ||
                        paramAnnotation.toString().contains("@cz.cvut.kbss.jopa.model.annotations.Properties")
                ) {
                    return true;
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
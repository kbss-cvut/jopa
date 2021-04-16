package cz.cvut.kbss.jopa.modelgen;

import com.sun.codemodel.JClass;
import cz.cvut.kbss.jopa.modelgen.Constants.Options;

import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.element.TypeElement;
import java.util.*;

/**
 * Represents context of the metamodel generator.
 */
public class Context {

    private final ProcessingEnvironment processingEnvironment;

    private final boolean debug;
    private final boolean addGenerationDate;
    private final boolean addGenerated;
    private final TypeElement generatedAnnotation;

    // Map of metamodel classes for entities and mapped superclasses
    private final Map<String, JClass> metaEntities = new HashMap<>();

    // keep track of all classes for which a model have been generated
    private final Set<String> generatedModelClasses = new HashSet<>();

    public Context(ProcessingEnvironment processingEnvironment) {
        this.processingEnvironment = processingEnvironment;
        this.debug = Boolean.parseBoolean(processingEnvironment.getOptions().getOrDefault(Options.DEBUG, Boolean.FALSE.toString()));
        this.addGenerationDate = Boolean.parseBoolean(processingEnvironment.getOptions().getOrDefault(Options.ADD_GENERATION_DATE, Boolean.FALSE.toString()));
        this.addGenerated = Boolean.parseBoolean(processingEnvironment.getOptions().getOrDefault(Options.ADD_GENERATED_ANNOTATION, Boolean.TRUE.toString()));
        TypeElement java8AndBelowGeneratedAnnotation =
                processingEnvironment.getElementUtils().getTypeElement("javax.annotation.Generated");
        if (java8AndBelowGeneratedAnnotation != null) {
            generatedAnnotation = java8AndBelowGeneratedAnnotation;
        } else {
            // Using the new name for this annotation in Java 9 and later
            generatedAnnotation = processingEnvironment.getElementUtils().getTypeElement("javax.annotation.processing.Generated");
        }
    }

    public boolean isDebug() {
        return debug;
    }

    public boolean shouldAddGenerated() {
        return addGenerated;
    }

    public boolean shouldAddGenerationDate() {
        return addGenerationDate;
    }

    public TypeElement getGeneratedAnnotation() {
        return generatedAnnotation;
    }

    public ProcessingEnvironment getProcessingEnvironment() {
        return processingEnvironment;
    }

    void markGenerated(String name) {
        generatedModelClasses.add(name);
    }

    boolean isAlreadyGenerated(String name) {
        return generatedModelClasses.contains(name);
    }

    public boolean containsMetaEntity(String qualifiedName) {
        return metaEntities.containsKey(qualifiedName);
    }

    public JClass getMetaEntity(String qualifiedName) {
        return metaEntities.get(qualifiedName);
    }

    public Collection<JClass> getMetaEntities() {
        return metaEntities.values();
    }

    public void addMetaEntity(String qualifiedName, JClass metaEntity) {
        metaEntities.put(qualifiedName, metaEntity);
    }
}

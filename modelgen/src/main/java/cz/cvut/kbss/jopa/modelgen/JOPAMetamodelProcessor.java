package cz.cvut.kbss.jopa.modelgen;

import cz.cvut.kbss.jopa.model.annotations.MappedSuperclass;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;

import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.RoundEnvironment;
import javax.lang.model.element.TypeElement;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import static cz.cvut.kbss.jopa.modelgen.Constants.Options.*;

/**
 * Java annotation processor used to generated static canonical metamodel for JOPA entities.
 */
public class JOPAMetamodelProcessor extends AbstractProcessor {

    @Override
    public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
        return false;
    }

    @Override
    public Set<String> getSupportedAnnotationTypes() {
        return new HashSet<>(Arrays.asList(OWLClass.class.getCanonicalName(), MappedSuperclass.class.getCanonicalName()));
    }

    @Override
    public Set<String> getSupportedOptions() {
        return new HashSet<>(Arrays.asList(DEBUG, ADD_GENERATION_DATE, ADD_GENERATED_ANNOTATION));
    }
}

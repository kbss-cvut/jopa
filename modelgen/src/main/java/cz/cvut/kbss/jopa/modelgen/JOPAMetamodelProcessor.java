package cz.cvut.kbss.jopa.modelgen;

import cz.cvut.kbss.jopa.model.annotations.MappedSuperclass;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.modelgen.util.TypeUtils;

import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.ProcessingEnvironment;
import javax.annotation.processing.RoundEnvironment;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.tools.Diagnostic;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import static cz.cvut.kbss.jopa.modelgen.Constants.Options.*;

/**
 * Java annotation processor used to generated static canonical metamodel for JOPA entities.
 */
public class JOPAMetamodelProcessor extends AbstractProcessor {

    private Context context;

    @Override
    public void init(ProcessingEnvironment env) {
        super.init(env);
        context = new Context(env);
        context.logMessage(Diagnostic.Kind.NOTE, "JOPA Static-Metamodel Generator " + Constants.VERSION);
    }

    @Override
    public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
        if ( roundEnv.processingOver() || annotations.size() == 0 ) {
            return false;
        }
        Set<? extends Element> elements = roundEnv.getRootElements();
        for ( Element element : elements ) {
            if ( isManagedType( element ) ) {
                context.logMessage( Diagnostic.Kind.OTHER, "Processing annotated class " + element.toString() );
//                handleRootElementAnnotationMirrors( element );
            }
        }

//        createMetaModelClasses();
        return false;
    }

    private boolean isManagedType(Element element) {
        return TypeUtils.containsAnnotation(element, OWLClass.class.getCanonicalName(), MappedSuperclass.class.getCanonicalName());
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

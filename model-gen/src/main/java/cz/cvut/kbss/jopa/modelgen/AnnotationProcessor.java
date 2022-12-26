package cz.cvut.kbss.jopa.modelgen;


import com.sun.codemodel.JClassAlreadyExistsException;
import com.sun.codemodel.JCodeModel;
import com.sun.codemodel.JDefinedClass;

import javax.annotation.processing.*;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.lang.model.util.Elements;
import javax.tools.Diagnostic;
import java.io.File;
import java.io.IOException;
import java.util.Set;

@SupportedAnnotationTypes({"cz.cvut.kbss.jopa.model.annotations.OWLClass", "cz.cvut.kbss.jopa.model.annotations.MappedSuperclass"})
public class AnnotationProcessor extends AbstractProcessor {
    Messager messager;
    private Elements elementUtils;

    private ProcessingEnvironment env;

    @Override
    public void init(ProcessingEnvironment env) {
        messager = env.getMessager();
        super.init(env);
        this.env = env;
        this.elementUtils = env.getElementUtils();
    }

    @Override
    public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
        JCodeModel codeModel = new JCodeModel();
        try {
            for (TypeElement te : annotations)
                for (Element e : roundEnv.getElementsAnnotatedWith(te)) {
                    messager.printMessage(Diagnostic.Kind.NOTE, "Printing: " + e.toString());
                    String packageName = elementUtils.getPackageOf(e).getQualifiedName().toString();
                    JDefinedClass definedClass = codeModel._class("./target/generated-sources/"+packageName + "." + e.getSimpleName() + "_");
                    codeModel.build(new File("."));
                }
        } catch (IOException e) {
            System.err.println("chybka");

        } catch (JClassAlreadyExistsException e) {
            System.err.println("chybka dva");

        }
        return true;
    }

    @Override
    public SourceVersion getSupportedSourceVersion() {
        return SourceVersion.latestSupported();
    }

    public ProcessingEnvironment getEnv() {
        return env;
    }
}
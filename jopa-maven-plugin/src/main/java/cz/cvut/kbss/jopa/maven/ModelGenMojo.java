package cz.cvut.kbss.jopa.maven;

import cz.cvut.kbss.jopa.modelgen.AnnotationProcessor;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;

import javax.annotation.processing.ProcessingEnvironment;
import javax.annotation.processing.RoundEnvironment;
import javax.lang.model.element.TypeElement;
import java.io.File;
import java.util.Collections;
import java.util.Set;


//not working
@Mojo(name = "gen-model")
public class ModelGenMojo extends AbstractMojo {
    private static final String OUTPUT_PARAM = "output-directory";
    private static final String SOURCE_PACKAGE_PARAM = "model-directory";







    @Parameter(property = "annotations")
    private Set<TypeElement> annotations;

    @Parameter(property = "roundEnv")
    private RoundEnvironment roundEnv;

    @Parameter(property = "processingEnv")
    private ProcessingEnvironment processingEnv;
    @Parameter(alias = OUTPUT_PARAM, defaultValue = "../target/generated-sources/")
    private String pOutputParam;

    @Parameter(alias = SOURCE_PACKAGE_PARAM)
    private String pSourcePackage;

    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {

        AnnotationProcessor processor = new AnnotationProcessor();

        Set<String> supportedAnnotationTypes = processor.getSupportedAnnotationTypes();

        Set<String> supportedOptions = processor.getSupportedOptions();

        ProcessingEnvironment processingEnv = new ProcessingEnvironment(processor, supportedAnnotationTypes, supportedOptions);

        processor.init(processingEnv);
        RoundEnvironment roundEnvironment = new RoundEnvironment.Builder()
                .setProcessors(Collections.singleton(processor))
                .setSupportedAnnotationTypes(supportedAnnotationTypes)
                .setSupportedOptions(supportedOptions)
                .build();
        processor.process(annotations,roundEnvironment);

    }


    private void printParameterValues() {
        print(OUTPUT_PARAM, pOutputParam);
        print(SOURCE_PACKAGE_PARAM, pSourcePackage);
    }
    private void print(String param, Object value) {
        getLog().info(param + ": " + value);
    }

    public void setProcessingEnv(ProcessingEnvironment processingEnv) {
        this.processingEnv = processingEnv;
    }

    public void setRoundEnv(RoundEnvironment roundEnv) {
        this.roundEnv = roundEnv;
    }
}
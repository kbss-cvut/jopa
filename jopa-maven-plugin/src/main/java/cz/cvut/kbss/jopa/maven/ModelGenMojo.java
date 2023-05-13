package cz.cvut.kbss.jopa.maven;

import cz.cvut.kbss.jopa.modelgen.ModelGenProcessor;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.DependencyResolutionRequiredException;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;

import javax.tools.*;
import java.io.File;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.*;

import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.apache.commons.lang3.StringUtils.join;


//not working
@Mojo(
        requiresDependencyResolution = ResolutionScope.COMPILE,
        defaultPhase = LifecyclePhase.GENERATE_SOURCES,
        name = "modelgen",
        requiresProject = true
)
public class ModelGenMojo extends AbstractMojo {

    private static final String OUTPUT_DIRECTORY_PARAM = "output-directory";
    private static final String ADDITIONAL_SORCES_PARAM = "additional-sources";
    private static final String SOURCE_PACKAGE_PARAM = "source-package";
    public static final String DEBUG_PARAM = "debug-option";

    //package ve kterým mám hledat soubory

    @Parameter(
            defaultValue = "${project}",
            readonly = true,
            required = true
    )
    private MavenProject project;
    @Parameter(alias = OUTPUT_DIRECTORY_PARAM, defaultValue = "./target/generated-sources/static-metamodel")
    private String outputDirectory;
    @Parameter(alias = SOURCE_PACKAGE_PARAM)
    private String sourcePackage;
    @Parameter(alias = DEBUG_PARAM, defaultValue = "false")
    private String debugOption;
    @Parameter(alias = ADDITIONAL_SORCES_PARAM, defaultValue = "")
    private String additionalSources;


    public void execute() throws MojoExecutionException {
        printParameterValues();
        getLog().info("");
        JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();

        List<String> options = new ArrayList<>();
        options.add("-processor");
        options.add(ModelGenProcessor.class.getName());

        final File[] classPathFiles = getClassPathFiles();

        final String compileClassPath = join(classPathFiles, File.pathSeparator);

        options.add("-cp");
        options.add(compileClassPath);

        options.add("-d");
        File folder = new File("./target/classes");
        if (!folder.exists()) folder.mkdirs();
        options.add("./target/classes");

        if (isNotBlank(outputDirectory)) {
            options.add("-AoutputDirectory=" + outputDirectory);
        }

        if (isNotBlank(sourcePackage)) {
            options.add("-AsourcePackage=" + sourcePackage);
        }

        if (isNotBlank(debugOption)) {
            options.add("-AdebugOption=" + debugOption);
        }

        StandardJavaFileManager fileManager = compiler.getStandardFileManager(null, null, null);

        DiagnosticCollector<JavaFileObject> diagnosticCollector = new DiagnosticCollector<>();

        List<File> sourceFiles = new ArrayList<>();

        for (File directory : getSourceDirectories()) {
            String dirname = directory.getPath();
            sourceFiles.addAll(findFiles(dirname));
        }
        Iterable<? extends JavaFileObject> compilationUnits =
                fileManager.getJavaFileObjectsFromFiles(sourceFiles);

        JavaCompiler.CompilationTask task = compiler.getTask(null, fileManager, diagnosticCollector, options, null, compilationUnits);

        getLog().info("Processing annotations began.");
        try {
            task.call();
        } catch (Exception e) {
            throw e;
        }
        for (Diagnostic<? extends JavaFileObject> diagnostic : diagnosticCollector.getDiagnostics()) {
            switch (diagnostic.getKind()){
                case ERROR:
                    getLog().error(diagnostic.getMessage(null));
                    break;
                case WARNING:
                    getLog().warn(diagnostic.getMessage(null));
                    break;
                case MANDATORY_WARNING:
                    getLog().warn(diagnostic.getMessage(null));
                    break;
                case NOTE:
                    getLog().info(diagnostic.getMessage(null));
                    break;
                case OTHER:
                    getLog().info(diagnostic.getMessage(null));
                    break;
            }
        }
        getLog().info("Static metamodel generated.");
        getLog().info("------------------------------------------------------------------------");
    }


    @SuppressWarnings("unchecked")
    private Set<File> getSourceDirectories() {
        File outputDirectory = new File(this.outputDirectory);
        String outputPath = outputDirectory.getAbsolutePath();
        Set<File> directories = new HashSet<File>();
        List<String> directoryNames = getCompileSourceRoots();
        for (String name : directoryNames) {
            File file = new File(name);
            if (file.exists() && file.isDirectory()) {
                directories.add(file);
            }
        }
        return directories;
    }


    private List<String> getCompileSourceRoots() {
        @SuppressWarnings("unchecked") final List<String> compileSourceRoots = project.getCompileSourceRoots();
        if (!additionalSources.isEmpty()) {
            compileSourceRoots.add(additionalSources);
        }
        return new ArrayList<String>(compileSourceRoots);
    }

    public List<File> findFiles(String directoryName) {
        List<File> resultList = new ArrayList<>();
        File directory = new File(directoryName);

        // Get all files from a directory.
        File[] fList = directory.listFiles();
        if (fList != null)
            for (File file : fList) {
                if (file.isFile()) {
                    resultList.add(file);
                } else if (file.isDirectory()) {
                    resultList.addAll(findFiles(file.getAbsolutePath()));
                }
            }
        return resultList;

    }


    private File[] getClassPathFiles() {
        final Set<File> files = new TreeSet<>(getCurrentClassPath());
        List<?> classpathElements;
        try {
            classpathElements = project.getTestClasspathElements();
        } catch (DependencyResolutionRequiredException e) {
            throw new RuntimeException(e.getMessage(), e);
        }

        for (final Object o : classpathElements) {
            if (o != null) {
                final File file = new File(o.toString());
                if (file.canRead()) {
                    files.add(file);
                }
            }
        }

        return files.toArray(new File[0]);
    }

    private List<File> getCurrentClassPath() {
        final List<File> retVal = new ArrayList<>();
        final URLClassLoader cl = (URLClassLoader) this.getClass().getClassLoader();
        try {
            for (URL url : cl.getURLs()) {
                retVal.add(new File(url.toURI()));
            }
            return retVal;
        } catch (URISyntaxException exc) {
            throw new RuntimeException(exc.getMessage(), exc);
        }
    }


    private void printParameterValues() {
        print(OUTPUT_DIRECTORY_PARAM, outputDirectory);
        print(SOURCE_PACKAGE_PARAM, sourcePackage);
        print(DEBUG_PARAM, debugOption);
        print(ADDITIONAL_SORCES_PARAM, additionalSources);
    }

    private void print(String param, Object value) {
        getLog().info(param + ": " + value);
    }
}
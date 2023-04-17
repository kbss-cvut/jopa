package cz.cvut.kbss.jopa.maven;

import com.github.jsonldjava.shaded.com.google.common.collect.Sets;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.DependencyResolutionRequiredException;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Component;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.Scanner;
import org.sonatype.plexus.build.incremental.BuildContext;

import javax.tools.*;
import java.io.File;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.nio.file.Files;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;


//not working
@Mojo(name = "gen-model")
public class ModelGenMojo extends AbstractMojo {
    private static final String JAVA_FILE_FILTER = "/*.java";

    private static final String[] ALL_JAVA_FILES_FILTER = new String[]{"**" + JAVA_FILE_FILTER};
    public static final String PLUGIN_PREFIX = "JOPA modelgen: ";
    private static final String OUTPUT_PARAM = "output-directory";
    private static final String SOURCE_PACKAGE_PARAM = "model-directory";

    private final String processor = cz.cvut.kbss.jopa.modelgen.AnnotationProcessor.class.getName();


    /**
     * @parameter
     */
    private boolean logOnlyOnError = false;

    /**
     * @component
     */
    @Component
    private BuildContext buildContext;

    @Parameter(defaultValue = "${project}", readonly = true, required = true)
    protected MavenProject project;

    /**
     * @parameter expression="${project.build.sourceEncoding}" required=true
     */
    private String sourceEncoding;

    /**
     * @parameter
     */
    private Map<String, String> options;

    /**
     * @parameter expression="${plugin.artifacts}" readonly=true required=true
     */
    private List<Artifact> pluginArtifacts;

    /**
     * @parameter
     */
    private Map<String, String> compilerOptions;

    /**
     * @parameter
     */
    private boolean showWarnings = false;

    @Parameter(defaultValue = "${project.build.directory}/generated-sources/static-metamodel/")
    private File outputDirectory;


    /**
     * @parameter
     */
    private boolean ignoreDelta = true;


    /**
     * A list of inclusion package filters for the apt processor.
     * <p>
     * If not specified all sources will be used for apt processor
     *
     * <pre>
     * e.g.:
     * &lt;includes&gt;
     * 	&lt;include&gt;com.mypackge.**.bo.**&lt;/include&gt;
     * &lt;/includes&gt;
     * </pre>
     * <p>
     * will include all files which match com/mypackge/ ** /bo/ ** / *.java
     *
     * @parameter
     */
    private Set<String> includes = new HashSet<String>();

    private List<String> buildCompilerOptions(String processor, String compileClassPath,
                                              String outputDirectory) throws IOException {
        Map<String, String> compilerOpts = new LinkedHashMap<String, String>();

        // Default options
        compilerOpts.put("cp", compileClassPath);

        if (sourceEncoding != null) {
            compilerOpts.put("encoding", sourceEncoding);
        }

        compilerOpts.put("proc:only", null);
        compilerOpts.put("processor", processor);

        if (options != null) {
            for (Map.Entry<String, String> entry : options.entrySet()) {
                if (entry.getValue() != null) {
                    compilerOpts.put("A" + entry.getKey() + "=" + entry.getValue(), null);
                } else {
                    compilerOpts.put("A" + entry.getKey() + "=", null);
                }

            }
        }

        if (outputDirectory != null) {
            compilerOpts.put("s", outputDirectory);
        }

        if (!showWarnings) {
            compilerOpts.put("nowarn", null);
        }

        StringBuilder builder = new StringBuilder();
        for (File file : getSourceDirectories()) {
            if (builder.length() > 0) {
                builder.append(";");
            }
            builder.append(file.getCanonicalPath());
        }
        compilerOpts.put("sourcepath", builder.toString());

        // User options override default options
        if (compilerOptions != null) {
            compilerOpts.putAll(compilerOptions);
        }

        List<String> opts = new ArrayList<String>(compilerOpts.size() * 2);

        for (Map.Entry<String, String> compilerOption : compilerOpts.entrySet()) {
            opts.add("-" + compilerOption.getKey());
            String value = compilerOption.getValue();
            if (StringUtils.isNotBlank(value)) {
                opts.add(value);
            }
        }
        return opts;
    }

    protected File getOutputDirectory() {
        return outputDirectory != null ? outputDirectory : new File("${project.build.directory}/generated-sources/model-gen");
    }

    @SuppressWarnings("unchecked")
    protected Set<File> getSourceDirectories() {
        File outputDirectory = getOutputDirectory();
        String outputPath = outputDirectory.getAbsolutePath();
        Set<File> directories = new HashSet<File>();
        List<String> directoryNames = project.getCompileSourceRoots();
        for (String name : directoryNames) {
            File file = new File(name);
            if (!file.getAbsolutePath().equals(outputPath) && file.exists() && file.isDirectory()) {
                directories.add(file);
            }
        }
        return directories;
    }

    private Set<File> filterFiles(Set<File> directories) {
        String[] filters = ALL_JAVA_FILES_FILTER;
        if (includes != null && !includes.isEmpty()) {
            filters = includes.toArray(new String[includes.size()]);
            for (int i = 0; i < filters.length; i++) {
                filters[i] = filters[i].replace('.', '/') + JAVA_FILE_FILTER;
            }
        }

        Set<File> files = new HashSet<File>();
        for (File directory : directories) {
            // support for incremental build in m2e context
            Scanner scanner = buildContext.newScanner(directory, false);
            scanner.setIncludes(filters);
            scanner.scan();
            String[] includedFiles = scanner.getIncludedFiles();

            // check also for possible deletions
            if (buildContext.isIncremental() && (includedFiles == null || includedFiles.length == 0)) {
                scanner = buildContext.newDeleteScanner(directory);
                scanner.setIncludes(filters);
                scanner.scan();
                includedFiles = scanner.getIncludedFiles();
            }

            // get all sources if ignoreDelta and at least one source file has changed
            if (ignoreDelta && buildContext.isIncremental() && includedFiles != null && includedFiles.length > 0) {
                scanner = buildContext.newScanner(directory, true);
                scanner.setIncludes(filters);
                scanner.scan();
                includedFiles = scanner.getIncludedFiles();
            }

            if (includedFiles != null) {
                for (String includedFile : includedFiles) {
                    files.add(new File(scanner.getBasedir(), includedFile));
                }
            }
        }
        return files;
    }

    /**
     * Add messages through the buildContext:
     * <ul>
     *   <li>cli build creates log output</li>
     *   <li>m2e build creates markers for eclipse</li>
     * </ul>
     *
     * @param diagnostics
     */
    private void processDiagnostics(final List<Diagnostic<? extends JavaFileObject>> diagnostics) {
        for (Diagnostic<? extends JavaFileObject> diagnostic : diagnostics) {
            JavaFileObject javaFileObject = diagnostic.getSource();
            if (javaFileObject != null) { // message was created without element parameter
                File file = new File(javaFileObject.toUri().getPath());
                Diagnostic.Kind kind = diagnostic.getKind();
                int lineNumber = (int) diagnostic.getLineNumber();
                int columnNumber = (int) diagnostic.getColumnNumber();
                String message = diagnostic.getMessage(Locale.getDefault());
                switch (kind) {
                    case ERROR:
                        buildContext.addMessage(file, lineNumber, columnNumber, message, BuildContext.SEVERITY_ERROR, null);
                        break;
                    case WARNING:
                    case MANDATORY_WARNING:
                        buildContext.addMessage(file, lineNumber, columnNumber, message, BuildContext.SEVERITY_WARNING, null);
                        break;
                    case NOTE:
                    case OTHER:
                    default:
                        break;
                }
            }
        }
    }

    @SuppressWarnings("unchecked")
    private String buildCompileClasspath() {
        List<String> pathElements = null;
        try {
            if (isForTest()) {
                pathElements = project.getTestClasspathElements();
            } else {
                pathElements = project.getCompileClasspathElements();
            }
        } catch (DependencyResolutionRequiredException e) {
            super.getLog().warn("exception calling getCompileClasspathElements", e);
            return null;
        }

        if (pluginArtifacts != null) {
            for (Artifact a : pluginArtifacts) {
                if (a.getFile() != null) {
                    pathElements.add(a.getFile().getAbsolutePath());
                }
            }
        }

        if (pathElements.isEmpty()) {
            return null;
        }

        StringBuilder result = new StringBuilder();
        int i = 0;
        for (i = 0; i < pathElements.size() - 1; ++i) {
            result.append(pathElements.get(i)).append(File.pathSeparatorChar);
        }
        result.append(pathElements.get(i));
        return result.toString();
    }

    public void execute() throws MojoExecutionException {
        if (getOutputDirectory() == null) {
            return;
        }
        if ("true".equals(System.getProperty("maven.apt.skip"))) {
            return;
        }

        if (!getOutputDirectory().exists()) {
            getOutputDirectory().mkdirs();
        }

        // make sure to add compileSourceRoots also during configuration build in m2e context
        if (isForTest()) {
            project.addTestCompileSourceRoot(getOutputDirectory().getAbsolutePath());
        } else {
            project.addCompileSourceRoot(getOutputDirectory().getAbsolutePath());
        }

        Set<File> sourceDirectories = getSourceDirectories();

        getLog().debug("Using build context: " + buildContext);

        StandardJavaFileManager fileManager = null;

        try {
            JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
            if (compiler == null) {
                throw new MojoExecutionException("You need to run build with JDK or have tools.jar on the classpath."
                        + "If this occures during eclipse build make sure you run eclipse under JDK as well");
            }

            Set<File> files = filterFiles(sourceDirectories);
            if (files.isEmpty()) {
                getLog().debug("No Java sources found (skipping)");
                return;
            }

            fileManager = compiler.getStandardFileManager(null, null, null);
            Iterable<? extends JavaFileObject> compilationUnits1 = fileManager.getJavaFileObjectsFromFiles(files);
            // clean all markers
            for (JavaFileObject javaFileObject : compilationUnits1) {
                buildContext.removeMessages(new File(javaFileObject.toUri().getPath()));
            }

            String compileClassPath = buildCompileClasspath();

            String outputDirectory = getOutputDirectory().getPath();
            File tempDirectory = null;

            if (buildContext.isIncremental()) {
                tempDirectory = new File(project.getBuild().getDirectory(), "apt" + System.currentTimeMillis());
                tempDirectory.mkdirs();
                outputDirectory = tempDirectory.getAbsolutePath();
            }

            List<String> compilerOptions = buildCompilerOptions(processor, compileClassPath, outputDirectory);

            Writer out = null;
            if (logOnlyOnError) {
                out = new StringWriter();
            }
            ExecutorService executor = Executors.newSingleThreadExecutor();
            try {
                DiagnosticCollector<JavaFileObject> diagnosticCollector = new DiagnosticCollector<JavaFileObject>();
                JavaCompiler.CompilationTask task = compiler.getTask(out, fileManager, diagnosticCollector, compilerOptions, null, compilationUnits1);
                Future<Boolean> future = executor.submit(task);
                Boolean rv = future.get();

                if (Boolean.FALSE.equals(rv) && logOnlyOnError) {
                    getLog().error(out.toString());
                }
                processDiagnostics(diagnosticCollector.getDiagnostics());
            } finally {
                executor.shutdown();
                if (tempDirectory != null) {
                    syncFiles(tempDirectory, getOutputDirectory());
                    FileUtils.deleteDirectory(tempDirectory);
                }
            }

            buildContext.refresh(getOutputDirectory());
        } catch (Exception e1) {
            getLog().error("execute error", e1);
            throw new MojoExecutionException(e1.getMessage(), e1);

        } finally {
            if (fileManager != null) {
                try {
                    fileManager.close();
                } catch (Exception e) {
                    getLog().warn("Unable to close fileManager", e);
                }
            }
        }
    }


    public static void syncFiles(File source, File target) throws IOException {
        Set<String> sourceFiles = Sets.newHashSet(source.list());
        Set<String> targetFiles = Sets.newHashSet(target.list());

        // remove files from target that are not in source
        for (String targetFile : targetFiles) {
            if (!sourceFiles.contains(targetFile)) {
                deleteFile(new File(target, targetFile));
            }
        }

        for (String sourceFile : sourceFiles) {
            File file = new File(source, sourceFile);
            File file2 = new File(target, sourceFile);
            if (file.isFile()) {
                copyIfChanged(file, file2);
            } else {
                file2.mkdir();
                syncFiles(file, file2);
            }
        }
    }


    private static void copyIfChanged(File source, File target) throws IOException {
        if (target.exists()) {
            if (source.length() == target.length() && FileUtils.checksumCRC32(source) == FileUtils.checksumCRC32(target)) {
                return;
            } else {
                target.delete();
            }
        }
        if (!source.renameTo(target)) {
            Files.move(source.toPath(), target.toPath());
        }
    }

    private static void deleteFile(File file) throws IOException {
        if (file.isDirectory()) {
            FileUtils.deleteDirectory(file);
        } else {
            file.delete();
        }
    }

    protected boolean isForTest() {
        return false;
    }

    private void printParameterValues() {
        print(OUTPUT_PARAM, outputDirectory);
    }

    private void print(String param, Object value) {
        getLog().info(param + ": " + value);
    }

    private void info(String msg) {
        getLog().info(PLUGIN_PREFIX + msg);
    }

    private void debug(String msg) {
        getLog().debug(PLUGIN_PREFIX + msg);
    }


    public void setBuildContext(BuildContext buildContext) {
        this.buildContext = buildContext;
    }

    public void setProject(MavenProject project) {
        this.project = project;
    }

    public void setSourceEncoding(String sourceEncoding) {
        this.sourceEncoding = sourceEncoding;
    }

    public void setOptions(Map<String, String> options) {
        this.options = options;
    }

    public void setCompilerOptions(Map<String, String> compilerOptions) {
        this.compilerOptions = compilerOptions;
    }

    public void setIncludes(Set<String> includes) {
        this.includes = includes;
    }

    public void setShowWarnings(boolean showWarnings) {
        this.showWarnings = showWarnings;
    }

    public void setLogOnlyOnError(boolean logOnlyOnError) {
        this.logOnlyOnError = logOnlyOnError;
    }

    public void setPluginArtifacts(List<Artifact> pluginArtifacts) {
        this.pluginArtifacts = pluginArtifacts;
    }
}
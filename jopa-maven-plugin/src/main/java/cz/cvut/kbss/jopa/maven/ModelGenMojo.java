/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.maven;

import cz.cvut.kbss.jopa.modelgen.ModelGenProcessor;
import org.apache.maven.artifact.DependencyResolutionRequiredException;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;

import javax.tools.Diagnostic;
import javax.tools.DiagnosticCollector;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.ToolProvider;
import java.io.File;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import static org.codehaus.plexus.util.StringUtils.isNotBlank;
import static org.codehaus.plexus.util.StringUtils.join;


@Mojo(
        requiresDependencyResolution = ResolutionScope.COMPILE,
        defaultPhase = LifecyclePhase.GENERATE_SOURCES,
        name = "modelgen"
)
public class ModelGenMojo extends AbstractMojo {

    private static final String OUTPUT_DIRECTORY_PARAM = "output-directory";
    private static final String ADDITIONAL_SOURCES_PARAM = "additional-sources";
    private static final String SOURCE_PACKAGE_PARAM = "source-package";
    public static final String DEBUG_PARAM = "debug-option";
    public static final String OUTPUT_PROPERTY_IRIS_PARAM = "output-property-iris";
    public static final String OUTPUT_IRI_AS_STRING_PARAM = "output-iri-as-string";
    public static final String INITIALIZE_IRIS_PARAM = "initialize-iris";

    @Parameter(defaultValue = "${project}", readonly = true, required = true)
    private MavenProject project;
    @Parameter(name = OUTPUT_DIRECTORY_PARAM, defaultValue = "./target/generated-sources/static-metamodel")
    private String outputDirectory;
    @Parameter(name = SOURCE_PACKAGE_PARAM)
    private String sourcePackage;
    @Parameter(name = DEBUG_PARAM, defaultValue = "false")
    private String debugOption;
    @Parameter(name = OUTPUT_PROPERTY_IRIS_PARAM, defaultValue = "false")
    private String outputPropertyIris;
    @Parameter(name = OUTPUT_IRI_AS_STRING_PARAM, defaultValue = "false")
    private String outputIriAsString;
    @Parameter(name = INITIALIZE_IRIS_PARAM, defaultValue = "false")
    private String initializeIris;
    @Parameter(name = ADDITIONAL_SOURCES_PARAM)
    private String additionalSources;


    public void execute() {
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
        if (!folder.exists()) {
            folder.mkdirs();
        }
        options.add("./target/classes");

        configureAnnotationProcessor(options);

        StandardJavaFileManager fileManager = compiler.getStandardFileManager(null, null, null);

        DiagnosticCollector<JavaFileObject> diagnosticCollector = new DiagnosticCollector<>();

        List<File> sourceFiles = new ArrayList<>();

        for (File directory : getSourceDirectories()) {
            sourceFiles.addAll(findFiles(directory.getPath()));
        }
        Iterable<? extends JavaFileObject> compilationUnits = fileManager.getJavaFileObjectsFromFiles(sourceFiles);

        JavaCompiler.CompilationTask task =
                compiler.getTask(null, fileManager, diagnosticCollector, options, null, compilationUnits);

        getLog().info("Processing annotations.");
        task.call();
        logTaskDiagnostics(diagnosticCollector);
        getLog().info("Static metamodel generated.");
        getLog().info("------------------------------------------------------------------------");
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

    private void configureAnnotationProcessor(List<String> options) {
        if (isNotBlank(outputDirectory)) {
            options.add("-AoutputDirectory=" + outputDirectory);
        }

        if (isNotBlank(sourcePackage)) {
            options.add("-AsourcePackage=" + sourcePackage);
        }

        if (isNotBlank(debugOption)) {
            options.add("-AdebugOption=" + debugOption);
        }

        if (isNotBlank(outputPropertyIris)) {
            options.add("-AoutputPropertyIris=" + outputPropertyIris);
        }

        if (isNotBlank(outputIriAsString)) {
            options.add("-AoutputIriAsString=" + outputIriAsString);
        }

        if (isNotBlank(initializeIris)) {
            options.add("-AinitializeIris=" + initializeIris);
        }
    }

    private Set<File> getSourceDirectories() {
        Set<File> directories = new HashSet<>();
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
        final List<String> compileSourceRoots = new ArrayList<>(project.getCompileSourceRoots());
        if (additionalSources != null && !additionalSources.isEmpty()) {
            compileSourceRoots.add(additionalSources);
        }
        return compileSourceRoots;
    }

    public List<File> findFiles(String directoryName) {
        List<File> resultList = new ArrayList<>();
        File directory = new File(directoryName);

        // Get all files from a directory.
        File[] fList = directory.listFiles();
        if (fList != null) {
            for (File file : fList) {
                if (file.isFile()) {
                    resultList.add(file);
                } else if (file.isDirectory()) {
                    resultList.addAll(findFiles(file.getAbsolutePath()));
                }
            }
        }
        return resultList;

    }

    private void logTaskDiagnostics(DiagnosticCollector<JavaFileObject> diagnosticCollector) {
        for (Diagnostic<? extends JavaFileObject> diagnostic : diagnosticCollector.getDiagnostics()) {
            switch (diagnostic.getKind()) {
                case ERROR:
                    getLog().error(diagnostic.getMessage(null));
                    break;
                case WARNING:   // Intentional fall-through
                case MANDATORY_WARNING:
                    getLog().warn(diagnostic.getMessage(null));
                    break;
                case NOTE:      // Intentional fall-through
                case OTHER:
                    getLog().info(diagnostic.getMessage(null));
                    break;
            }
        }
    }

    private void printParameterValues() {
        Utils.logParameterValue(OUTPUT_DIRECTORY_PARAM, outputDirectory, getLog());
        Utils.logParameterValue(SOURCE_PACKAGE_PARAM, sourcePackage, getLog());
        Utils.logParameterValue(DEBUG_PARAM, debugOption, getLog());
        Utils.logParameterValue(ADDITIONAL_SOURCES_PARAM, additionalSources, getLog());
        Utils.logParameterValue(OUTPUT_PROPERTY_IRIS_PARAM, outputPropertyIris, getLog());
        Utils.logParameterValue(OUTPUT_IRI_AS_STRING_PARAM, outputIriAsString, getLog());
    }
}

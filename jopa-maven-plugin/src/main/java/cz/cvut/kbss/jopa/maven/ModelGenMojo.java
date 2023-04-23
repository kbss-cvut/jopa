package cz.cvut.kbss.jopa.maven;

import cz.cvut.kbss.jopa.modelgen.ModelGenProcessor;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;

import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.ToolProvider;
import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;


//not working
@Mojo(name = "gen-model")
public class ModelGenMojo extends AbstractMojo {

    private static final String OUTPUT_PARAM = "output-directory";


    /**
     * @parameter expression="${project}" readonly=true required=true
     */
    @Parameter(defaultValue = "${project}")
    private MavenProject project;
    @Parameter(defaultValue = "./target/generated-sources/static-metamodel")
    private String outputDirectory;

    public void execute() throws MojoExecutionException {
        JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();

        List<String> options = new ArrayList<>();
        options.add("-processor");
        options.add(ModelGenProcessor.class.getName());

        StandardJavaFileManager fileManager = compiler.getStandardFileManager(null, null, null);

        List<File> sourceFiles = new ArrayList<>();

        for (File directory : getSourceDirectories()) {
            String dirname = directory.getPath();
            sourceFiles.addAll(findFiles(dirname));
        }
        Iterable<? extends JavaFileObject> compilationUnits =
                fileManager.getJavaFileObjectsFromFiles(sourceFiles);

        JavaCompiler.CompilationTask task = compiler.getTask(null, fileManager, null, options, null, compilationUnits);
        try {
            task.call();
            System.out.println("xd");
        } catch (Exception e) {
            throw e;
        }
    }


    @SuppressWarnings("unchecked")
    private Set<File> getSourceDirectories() {
        File outputDirectory = new File(this.outputDirectory);
        String outputPath = outputDirectory.getAbsolutePath();
        Set<File> directories = new HashSet<File>();
        List<String> directoryNames = getCompileSourceRoots();
        for (String name : directoryNames) {
            File file = new File(name);
            if (!file.getAbsolutePath().equals(outputPath) && file.exists() && file.isDirectory()) {
                directories.add(file);
            }
        }
        return directories;
    }


    private List<String> getCompileSourceRoots() {
        @SuppressWarnings("unchecked") final List<String> compileSourceRoots = project.getCompileSourceRoots();
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

}
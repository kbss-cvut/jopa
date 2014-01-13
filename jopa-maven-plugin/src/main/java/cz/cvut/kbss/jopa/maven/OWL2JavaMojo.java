package cz.cvut.kbss.jopa.maven;

import cz.cvut.kbss.jopa.owl2java.OWL2JavaTransformer;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;

@Mojo( name = "owl2java-transform" )
public class OWL2JavaMojo extends AbstractMojo {

    @Parameter(alias="mapping-file")
    private String pMappingFile;

    @Parameter(alias="package")
    private String pPackage;

    @Parameter(alias = "context-name")
    private String pContextName;

    @Parameter(alias = "ontology-iri")
    private String pOntologyIRI;

    @Parameter(alias = "output-directory")
    private String pOutputDirectory;

    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {
        OWL2JavaTransformer owl2java = new OWL2JavaTransformer();

        System.out.println(pMappingFile + ","
                + pPackage + ","
                + pContextName + ","
                + pOntologyIRI + ","
                + pOutputDirectory + ","
        );

        if ( pOntologyIRI == null ) {
            getLog().error("The parameter 'ontology-iri' is invalid. Must not be null.");
            getLog().error("Skipping OWL2Java transformation.");
            return;
        }

        if ( pMappingFile != null && !pMappingFile.isEmpty() ) {
            owl2java.setOntology(pOntologyIRI, pMappingFile, true);
        } else {
            owl2java.setOntology(pOntologyIRI, null, true);
        }

        if (!owl2java.listContexts().contains(pContextName)) {
            getLog().error("The parameter '-c' is invalid. Found contexts: "
                    + owl2java.listContexts());
            getLog().error("Skipping OWL2Java transformation.");
            return;
        }

        owl2java.transform(pContextName,
                pPackage, pOutputDirectory);

        getLog().info( "OWL2Java successfully generated!" );
    }
}

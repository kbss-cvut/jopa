/**
 * Copyright (C) 2016 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
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

    @Parameter(alias = "with-owlapi",defaultValue = "false")
    private Boolean pWithOWLAPI;

    @Parameter(alias = "whole-ontology-as-ics",defaultValue = "false")
    private Boolean pWholeOntologyAsICS;

    @Parameter(alias = "vocabulary-only",defaultValue = "false")
    private Boolean pVocabularyOnly;

    @Override
    public void execute() {
        OWL2JavaTransformer owl2java = new OWL2JavaTransformer();

        System.out.println(pWholeOntologyAsICS + ","
                + pMappingFile + ","
                + pPackage + ","
                + pContextName + ","
                + pOntologyIRI + ","
                + pOutputDirectory + ","
                + pVocabularyOnly + ","
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

        if ( pVocabularyOnly ) {
            owl2java.generateVocabulary(pWholeOntologyAsICS ? null : pContextName,
                    pPackage, pOutputDirectory, pWithOWLAPI);
        } else {
            owl2java.transform(pWholeOntologyAsICS ? null : pContextName,
                    pPackage, pOutputDirectory, pWithOWLAPI);
        }

        getLog().info( "OWL2Java successfully generated!" );
    }
}

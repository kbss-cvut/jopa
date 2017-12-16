/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.maven;

import cz.cvut.kbss.jopa.owl2java.OWL2JavaTransformer;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;

@Mojo(name = "owl2java-transform")
public class OWL2JavaMojo extends AbstractMojo {

    private static final String MAPPING_FILE_PARAM = "mapping-file";
    private static final String PACKAGE_PARAM = "package";
    private static final String CONTEXT_PARAM = "context-name";
    private static final String ONTOLOGY_PARAM = "ontology-iri";
    private static final String OUTPUT_PARAM = "output-directory";
    private static final String W_OWLAPI_PARAM = "with-owlapi";
    private static final String ALL_IC_PARAM = "whole-ontology-as-ics";
    private static final String VOCABULARY_PARAM = "vocabulary-only";
    private static final String IGNORE_FAILED_IMPORTS_PARAM = "ignore-failed-imports";

    @Parameter(alias = MAPPING_FILE_PARAM)
    private String pMappingFile;

    @Parameter(alias = PACKAGE_PARAM)
    private String pPackage;

    @Parameter(alias = CONTEXT_PARAM)
    private String pContextName;

    @Parameter(alias = ONTOLOGY_PARAM)
    private String pOntologyIRI;

    @Parameter(alias = OUTPUT_PARAM)
    private String pOutputDirectory;

    @Parameter(alias = W_OWLAPI_PARAM, defaultValue = "false")
    private boolean pWithOWLAPI;

    @Parameter(alias = ALL_IC_PARAM, defaultValue = "false")
    private boolean pWholeOntologyAsICS;

    @Parameter(alias = VOCABULARY_PARAM, defaultValue = "false")
    private boolean pVocabularyOnly;

    @Parameter(alias = IGNORE_FAILED_IMPORTS_PARAM, defaultValue = "false")
    private boolean ignoreFailedImports;

    @Override
    public void execute() {
        OWL2JavaTransformer owl2java = new OWL2JavaTransformer();

        printParameterValues();

        if (pOntologyIRI == null) {
            getLog().error("The parameter 'ontology-iri' is invalid. Must not be null.");
            getLog().error("Skipping OWL2Java transformation.");
            return;
        }
        owl2java.ignoreMissingImports(ignoreFailedImports);

        if (pMappingFile != null && !pMappingFile.isEmpty()) {
            owl2java.setOntology(pOntologyIRI, pMappingFile, true);
        } else {
            owl2java.setOntology(pOntologyIRI, null, true);
        }

        if (pVocabularyOnly) {
            owl2java.generateVocabulary(pWholeOntologyAsICS ? null : pContextName,
                    pPackage, pOutputDirectory, pWithOWLAPI);
        } else {
            owl2java.transform(pWholeOntologyAsICS ? null : pContextName,
                    pPackage, pOutputDirectory, pWithOWLAPI);
        }

        getLog().info("OWL2Java successfully generated!");
    }

    private void printParameterValues() {
        getLog().info(MAPPING_FILE_PARAM + ": " + pMappingFile);
        getLog().info(PACKAGE_PARAM + ": " + pPackage);
        getLog().info(CONTEXT_PARAM + ": " + pContextName);
        getLog().info(ONTOLOGY_PARAM + ": " + pOntologyIRI);
        getLog().info(OUTPUT_PARAM + ": " + pOutputDirectory);
        getLog().info(W_OWLAPI_PARAM + ": " + pWithOWLAPI);
        getLog().info(ALL_IC_PARAM + ": " + pWholeOntologyAsICS);
        getLog().info(VOCABULARY_PARAM + ": " + pVocabularyOnly);
        getLog().info(IGNORE_FAILED_IMPORTS_PARAM + ": " + ignoreFailedImports);
    }
}

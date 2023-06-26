/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.owl2java.cli.PropertiesType;
import cz.cvut.kbss.jopa.owl2java.config.TransformationConfiguration;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;

@Mojo(name = "owl2java-transform", defaultPhase = LifecyclePhase.GENERATE_SOURCES)
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
    private static final String PROPERTIES_TYPE = "properties-type";
    private static final String GENERATE_JAVADOC = "javadoc-from-rdfs-comment";
    private static final String PREFER_MULTILINGUAL_STRINGS = "prefer-multilingual-strings";
    private static final String GENERATE_ANNOTATION_FIELDS = "generate-annotation-fields";
    private static final String GENERATE_THING = "generate-thing";

    @Parameter(alias = MAPPING_FILE_PARAM)
    private String pMappingFile;

    @Parameter(alias = PACKAGE_PARAM, required = true)
    private String pPackage;

    @Parameter(alias = CONTEXT_PARAM)
    private String pContextName;

    @Parameter(alias = ONTOLOGY_PARAM)
    private String pOntologyIRI;

    @Parameter(alias = OUTPUT_PARAM, defaultValue = "${project.basedir}/src/main/generated-sources")
    private String pOutputDirectory;

    @Parameter(alias = W_OWLAPI_PARAM, defaultValue = "false")
    private boolean pWithOWLAPI;

    @Parameter(alias = ALL_IC_PARAM, defaultValue = "false")
    private boolean pWholeOntologyAsICS;

    @Parameter(alias = VOCABULARY_PARAM, defaultValue = "false")
    private boolean pVocabularyOnly;

    @Parameter(alias = IGNORE_FAILED_IMPORTS_PARAM, defaultValue = "false")
    private boolean ignoreFailedImports;

    @Parameter(alias = PROPERTIES_TYPE, defaultValue = "string")
    private String pPropertiesType;

    @Parameter(alias = GENERATE_JAVADOC, defaultValue = "true")
    private boolean generateJavadoc;

    @Parameter(alias = PREFER_MULTILINGUAL_STRINGS, defaultValue = "true")
    private boolean preferMultilingualStrings;

    @Parameter(alias = GENERATE_ANNOTATION_FIELDS, defaultValue = "true")
    private boolean generateAnnotationFields;

    @Parameter(alias = GENERATE_THING, defaultValue = "true")
    private boolean generateThing;

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
            owl2java.setOntology(pOntologyIRI, pMappingFile);
        } else {
            owl2java.setOntology(pOntologyIRI, null);
        }

        final TransformationConfiguration.TransformationConfigurationBuilder builder =
                TransformationConfiguration.builder();
        if (!pWholeOntologyAsICS) {
            builder.context(pContextName);
        }

        if (pPropertiesType != null) {
            builder.propertiesType(PropertiesType.valueOf(pPropertiesType));
        }

        final TransformationConfiguration config =
                builder.packageName(pPackage).targetDir(pOutputDirectory).addOwlapiIris(pWithOWLAPI)
                       .generateJavadoc(generateJavadoc).preferMultilingualStrings(preferMultilingualStrings)
                       .generateAnnotationFields(generateAnnotationFields).generateThing(generateThing).build();

        if (pVocabularyOnly) {
            owl2java.generateVocabulary(config);
        } else {
            owl2java.transform(config);
        }

        getLog().info("OWL2Java successfully generated!");
    }

    private void printParameterValues() {
        Utils.logParameterValue(MAPPING_FILE_PARAM, pMappingFile, getLog());
        Utils.logParameterValue(PACKAGE_PARAM, pPackage, getLog());
        Utils.logParameterValue(CONTEXT_PARAM, pContextName, getLog());
        Utils.logParameterValue(ONTOLOGY_PARAM, pOntologyIRI, getLog());
        Utils.logParameterValue(OUTPUT_PARAM, pOutputDirectory, getLog());
        Utils.logParameterValue(W_OWLAPI_PARAM, pWithOWLAPI, getLog());
        Utils.logParameterValue(ALL_IC_PARAM, pWholeOntologyAsICS, getLog());
        Utils.logParameterValue(VOCABULARY_PARAM, pVocabularyOnly, getLog());
        Utils.logParameterValue(IGNORE_FAILED_IMPORTS_PARAM, ignoreFailedImports, getLog());
        Utils.logParameterValue(PROPERTIES_TYPE,pPropertiesType, getLog());
        Utils.logParameterValue(GENERATE_JAVADOC, generateJavadoc, getLog());
        Utils.logParameterValue(PREFER_MULTILINGUAL_STRINGS, preferMultilingualStrings, getLog());
        Utils.logParameterValue(GENERATE_ANNOTATION_FIELDS, generateAnnotationFields, getLog());
        Utils.logParameterValue(GENERATE_THING, generateThing, getLog());
    }
}

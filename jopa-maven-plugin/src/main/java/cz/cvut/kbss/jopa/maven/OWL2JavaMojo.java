/**
 * Copyright (C) 2023 Czech Technical University in Prague
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

    @Parameter(name = MAPPING_FILE_PARAM)
    private String mappingFile;

    @Parameter(name = PACKAGE_PARAM, required = true)
    private String pPackage;

    @Parameter(name = CONTEXT_PARAM)
    private String contextName;

    @Parameter(name = ONTOLOGY_PARAM)
    private String ontologyIri;

    @Parameter(name = OUTPUT_PARAM, defaultValue = "${project.basedir}/src/main/generated-sources")
    private String outputDirectory;

    @Parameter(name = W_OWLAPI_PARAM, defaultValue = "false")
    private boolean withOwlapi;

    @Parameter(name = ALL_IC_PARAM, defaultValue = "false")
    private boolean wholeOntologyAsIcs;

    @Parameter(name = VOCABULARY_PARAM, defaultValue = "false")
    private boolean vocabularyOnly;

    @Parameter(name = IGNORE_FAILED_IMPORTS_PARAM, defaultValue = "false")
    private boolean ignoreFailedImports;

    @Parameter(name = PROPERTIES_TYPE, defaultValue = "string")
    private String propertiesType;

    @Parameter(name = GENERATE_JAVADOC, defaultValue = "true")
    private boolean javadocFromRdfsComment;

    @Parameter(name = PREFER_MULTILINGUAL_STRINGS, defaultValue = "true")
    private boolean preferMultilingualStrings;

    @Parameter(name = GENERATE_ANNOTATION_FIELDS, defaultValue = "true")
    private boolean generateAnnotationFields;

    @Parameter(name = GENERATE_THING, defaultValue = "true")
    private boolean generateThing;

    @Override
    public void execute() {
        OWL2JavaTransformer owl2java = new OWL2JavaTransformer();

        printParameterValues();

        if (ontologyIri == null) {
            getLog().error("The parameter 'ontology-iri' is invalid. Must not be null.");
            getLog().error("Skipping OWL2Java transformation.");
            return;
        }
        owl2java.ignoreMissingImports(ignoreFailedImports);

        if (mappingFile != null && !mappingFile.isEmpty()) {
            owl2java.setOntology(ontologyIri, mappingFile);
        } else {
            owl2java.setOntology(ontologyIri, null);
        }

        final TransformationConfiguration.TransformationConfigurationBuilder builder =
                TransformationConfiguration.builder();
        if (!wholeOntologyAsIcs) {
            builder.context(contextName);
        }

        if (propertiesType != null) {
            builder.propertiesType(PropertiesType.valueOf(propertiesType));
        }

        final TransformationConfiguration config =
                builder.packageName(pPackage).targetDir(outputDirectory).addOwlapiIris(withOwlapi)
                       .generateJavadoc(javadocFromRdfsComment).preferMultilingualStrings(preferMultilingualStrings)
                       .generateAnnotationFields(generateAnnotationFields).generateThing(generateThing).build();

        if (vocabularyOnly) {
            owl2java.generateVocabulary(config);
        } else {
            owl2java.transform(config);
        }

        getLog().info("OWL2Java successfully generated!");
    }

    private void printParameterValues() {
        Utils.logParameterValue(MAPPING_FILE_PARAM, mappingFile, getLog());
        Utils.logParameterValue(PACKAGE_PARAM, pPackage, getLog());
        Utils.logParameterValue(CONTEXT_PARAM, contextName, getLog());
        Utils.logParameterValue(ONTOLOGY_PARAM, ontologyIri, getLog());
        Utils.logParameterValue(OUTPUT_PARAM, outputDirectory, getLog());
        Utils.logParameterValue(W_OWLAPI_PARAM, withOwlapi, getLog());
        Utils.logParameterValue(ALL_IC_PARAM, wholeOntologyAsIcs, getLog());
        Utils.logParameterValue(VOCABULARY_PARAM, vocabularyOnly, getLog());
        Utils.logParameterValue(IGNORE_FAILED_IMPORTS_PARAM, ignoreFailedImports, getLog());
        Utils.logParameterValue(PROPERTIES_TYPE, propertiesType, getLog());
        Utils.logParameterValue(GENERATE_JAVADOC, javadocFromRdfsComment, getLog());
        Utils.logParameterValue(PREFER_MULTILINGUAL_STRINGS, preferMultilingualStrings, getLog());
        Utils.logParameterValue(GENERATE_ANNOTATION_FIELDS, generateAnnotationFields, getLog());
        Utils.logParameterValue(GENERATE_THING, generateThing, getLog());
    }

    public String getPackage() {
        return pPackage;
    }

    public void setPackage(String pPackage) {
        this.pPackage = pPackage;
    }
}

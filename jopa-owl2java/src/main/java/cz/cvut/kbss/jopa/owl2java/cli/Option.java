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
package cz.cvut.kbss.jopa.owl2java.cli;

/**
 * Command line options for configuring the transformation/vocabulary generation.
 */
public enum Option {
    /**
     * Mapping file path.
     * <p>
     * Mapping file allows to map logical ontology IRI to its physical location (e.g. IRI to a local file path)
     */
    MAPPING_FILE("m", "mapping file"),
    /**
     * Name of context which should be processed by the tool.
     * <p>
     * Context is specified by annotating an axiom with {@link cz.cvut.kbss.jopa.owl2java.Constants#P_IS_INTEGRITY_CONSTRAINT_FOR}
     * containing the name of the context. When it is specified, only axioms annotated with the context name are
     * considered for transformation/vocabulary generation.
     */
    CONTEXT("c", "context name"),
    /**
     * Whether to generate vocabulary constants as instances of OWLAPI {@link org.semanticweb.owlapi.model.IRI}.
     * <p>
     * By default, they are String.
     */
    WITH_IRIS("w", "with OWLAPI IRIs"),
    /**
     * Output directory name.
     * <p>
     * Note that the generator will create subdirectories corresponding to the package into which the target classes are
     * generated.
     */
    TARGET_DIR("d", "output directory"),
    /**
     * Target package for the generated classes.
     * <p>
     * Note that when generating the model, a {@link cz.cvut.kbss.jopa.owl2java.Constants#MODEL_PACKAGE} subpackage is
     * put into the configured package for model classes. E.g., when generating into {@code cz.cvut.kbss}, the {@code
     * Vocabulary} will be in this package, but the model classes will be in {@code cz.cvut.kbss.model}.
     */
    PACKAGE("p", "package"),
    /**
     * Whether to interpret the whole ontology as integrity constraints.
     * <p>
     * This supersedes {@link #CONTEXT} and processes all axioms in the ontology.
     */
    WHOLE_ONTOLOGY_AS_IC("i",
            "interpret whole ontology as integrity constraints; this option supersedes the '-c' option."),
    /**
     * Whether to ignore failed imports.
     * <p>
     * By default, the transformation/vocabulary generation will fail if an import cannot be resolved.
     */
    IGNORE_FAILED_IMPORTS("f", "ignore failed ontology imports"),
    /**
     * Configures annotation whose value should be used as name of the target Java class.
     * <p>
     * By default, the Java class name is derived from the class IRI.
     */
    JAVA_CLASSNAME_ANNOTATION("jca", "java class name annotation"),
    /**
     * Configures whether properties with RDF langString range should be mapped to {@link
     * cz.cvut.kbss.jopa.model.MultilingualString} typed fields.
     * <p>
     * Defaults to {@link cz.cvut.kbss.jopa.owl2java.config.Defaults#PREFER_MULTILINGUAL_STRINGS}. If false, the fields
     * will have type {@link String}.
     */
    PREFER_MULTILINGUAL_STRINGS("langStrings", "prefer multilingual strings"),
    /**
     * Generic type of the {@link cz.cvut.kbss.jopa.model.annotations.Properties} attribute.
     * <p>
     * Possible options are {@link PropertiesType#string} for {@code <String, Set<String>>} or {@link
     * PropertiesType#object} for {@code <String, Set<Object>>}.
     */
    PROPERTIES_TYPE("pt", "type of the @Properties map value set - string (default), object"),
    /**
     * Whether to generate Javadoc from values of {@code rdfs:comment} annotations.
     * <p>
     * By default, OWL2Java will generate Javadoc from the first rdfs:comment it finds for each axiom. This leads to
     * documentation being created for classes, their attributes and vocabulary constants.
     */
    GENERATE_JAVADOC_FROM_COMMENT("doc", "generate Javadoc from rdfs:comment annotations"),

    /**
     * Whether to automatically generate name ({@literal rdfs:label}) and description ({@literal dc:description}) fields.
     */
    GENERATE_ANNOTATION_FIELDS("ann", "automatically generate rdfs:label and dc:description attributes for all entities"),

    /**
     * Whether to automatically generate an entity class corresponding to {@literal owl:Thing}.
     */
    GENERATE_THING("thing", "automatically generate an entity class corresponding to owl:Thing");

    public final String arg;
    final String description;

    Option(String arg, String description) {
        this.arg = arg;
        this.description = description;
    }
}

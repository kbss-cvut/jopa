/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.owl2java.config;

import cz.cvut.kbss.jopa.owl2java.cli.CliParams;
import cz.cvut.kbss.jopa.owl2java.cli.Option;
import cz.cvut.kbss.jopa.owl2java.cli.PropertiesType;

public class TransformationConfiguration {

    private final String context;

    private final String packageName;

    private final String targetDir;

    private final boolean generateOwlapiIris;

    private final boolean generateJavadoc;

    private final boolean preferMultilingualStrings;

    private final PropertiesType propertiesType;

    private final boolean generateAnnotationFields;

    private final boolean generateThing;

    private final CliParams cliParams;

    private TransformationConfiguration(TransformationConfigurationBuilder builder) {
        this.context = builder.context;
        this.packageName = builder.packageName;
        this.targetDir = builder.targetDir;
        this.generateOwlapiIris = builder.owlapiIris;
        this.generateJavadoc = builder.generateJavadoc;
        this.preferMultilingualStrings = builder.preferMultilingualStrings;
        this.propertiesType = builder.propertiesType;
        this.generateAnnotationFields = builder.generateAnnotationFields;
        this.generateThing = builder.generateThing;
        this.cliParams = CliParams.empty();
    }

    private TransformationConfiguration(CliParams cliParams) {
        this.cliParams = cliParams;
        this.context = cliParams.is(Option.WHOLE_ONTOLOGY_AS_IC.arg) ? null : cliParams.valueOf(Option.CONTEXT.arg)
                                                                                       .toString();
        this.packageName = cliParams.valueOf(Option.PACKAGE.arg).toString();
        this.targetDir = cliParams.valueOf(Option.TARGET_DIR.arg).toString();
        this.generateOwlapiIris = cliParams.is(Option.WITH_IRIS.arg, Defaults.WITH_IRIS);
        this.generateJavadoc = cliParams.is(Option.GENERATE_JAVADOC_FROM_COMMENT.arg, Defaults.GENERATE_JAVADOC_FROM_COMMENT);
        this.preferMultilingualStrings = cliParams.is(Option.PREFER_MULTILINGUAL_STRINGS.arg, Defaults.PREFER_MULTILINGUAL_STRINGS);
        this.propertiesType = PropertiesType.fromParam(cliParams.valueOf(Option.PROPERTIES_TYPE.arg));
        this.generateAnnotationFields = cliParams.is(Option.GENERATE_ANNOTATION_FIELDS.arg, Defaults.GENERATE_ANNOTATION_FIELDS);
        this.generateThing = cliParams.is(Option.GENERATE_THING.arg, Defaults.GENERATE_THING);
    }

    public String getContext() {
        return context;
    }

    public String getPackageName() {
        return packageName;
    }

    public String getTargetDir() {
        return targetDir;
    }

    public boolean areAllAxiomsIntegrityConstraints() {
        return context == null;
    }

    public boolean shouldGenerateOwlapiIris() {
        return generateOwlapiIris;
    }

    public boolean shouldGenerateJavadoc() {
        return generateJavadoc;
    }

    public boolean shouldPreferMultilingualStrings() {
        return preferMultilingualStrings;
    }

    public PropertiesType getPropertiesType() {
        return propertiesType;
    }

    public boolean shouldGenerateAnnotationFields() {
        return generateAnnotationFields;
    }

    public boolean shouldGenerateThing() {
        return generateThing;
    }

    public CliParams getCliParams() {
        return cliParams;
    }

    public static TransformationConfiguration config(CliParams cliParams) {
        return new TransformationConfiguration(cliParams);
    }

    public static TransformationConfigurationBuilder builder() {
        return new TransformationConfigurationBuilder();
    }

    public static class TransformationConfigurationBuilder {
        private String context;
        private String packageName = Defaults.PACKAGE;
        private String targetDir = Defaults.TARGET_DIR;
        private PropertiesType propertiesType = PropertiesType.valueOf(Defaults.PROPERTIES_TYPE);
        private boolean owlapiIris = Defaults.WITH_IRIS;
        private boolean generateJavadoc = Defaults.GENERATE_JAVADOC_FROM_COMMENT;
        private boolean preferMultilingualStrings = Defaults.PREFER_MULTILINGUAL_STRINGS;
        private boolean generateAnnotationFields = Defaults.GENERATE_ANNOTATION_FIELDS;
        private boolean generateThing = Defaults.GENERATE_THING;

        public TransformationConfigurationBuilder context(String context) {
            this.context = context;
            return this;
        }

        public TransformationConfigurationBuilder packageName(String packageName) {
            this.packageName = packageName;
            return this;
        }

        public TransformationConfigurationBuilder targetDir(String targetDir) {
            this.targetDir = targetDir;
            return this;
        }

        public TransformationConfigurationBuilder addOwlapiIris(boolean add) {
            this.owlapiIris = add;
            return this;
        }

        public TransformationConfigurationBuilder generateJavadoc(boolean javadoc) {
            this.generateJavadoc = javadoc;
            return this;
        }

        public TransformationConfigurationBuilder preferMultilingualStrings(boolean preferMultilingualStrings) {
            this.preferMultilingualStrings = preferMultilingualStrings;
            return this;
        }

        public TransformationConfigurationBuilder propertiesType(PropertiesType propertiesType) {
            this.propertiesType = propertiesType;
            return this;
        }

        public TransformationConfigurationBuilder generateAnnotationFields(boolean generateAnnotationFields) {
            this.generateAnnotationFields = generateAnnotationFields;
            return this;
        }

        public TransformationConfigurationBuilder generateThing(boolean generateThing) {
            this.generateThing = generateThing;
            return this;
        }

        public TransformationConfiguration build() {
            return new TransformationConfiguration(this);
        }
    }
}

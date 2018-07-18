package cz.cvut.kbss.jopa.owl2java.config;

import cz.cvut.kbss.jopa.owl2java.cli.CommandParserProvider;
import cz.cvut.kbss.jopa.owl2java.cli.Option;
import cz.cvut.kbss.jopa.owl2java.cli.PropertiesType;
import joptsimple.OptionSet;

public class TransformationConfiguration {

    private final String context;

    private final String packageName;

    private final String targetDir;

    private final boolean generateOwlapiIris;

    private final boolean generateJavadoc;

    private final PropertiesType propertiesType;

    private final OptionSet cliParams;

    private TransformationConfiguration(TransformationConfigurationBuilder builder) {
        this.context = builder.context;
        this.packageName = builder.packageName;
        this.targetDir = builder.targetDir;
        this.generateOwlapiIris = builder.owlapiIris;
        this.generateJavadoc = builder.generateJavadoc;
        this.propertiesType = builder.propertiesType;
        this.cliParams = CommandParserProvider.getCommandTransform().parse("");
    }

    private TransformationConfiguration(OptionSet cliParams) {
        this.cliParams = cliParams;
        this.context = (Boolean) cliParams.valueOf(Option.WHOLE_ONTOLOGY_AS_IC.arg) ?
                       cliParams.valueOf(Option.CONTEXT.arg).toString() : null;
        this.packageName = cliParams.valueOf(Option.PACKAGE.arg).toString();
        this.targetDir = cliParams.valueOf(Option.TARGET_DIR.arg).toString();
        this.generateOwlapiIris = (Boolean) cliParams.valueOf(Option.WITH_IRIS.arg);
        this.generateJavadoc = (Boolean) cliParams.valueOf(Option.GENERATE_JAVADOC_FROM_COMMENT.arg);
        this.propertiesType = PropertiesType.fromParam(cliParams.valueOf(Option.PROPERTIES_TYPE.arg));
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

    public PropertiesType getPropertiesType() {
        return propertiesType;
    }

    public OptionSet getCliParams() {
        return cliParams;
    }

    public static TransformationConfiguration config(OptionSet cliParams) {
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

        public TransformationConfigurationBuilder propertiesType(PropertiesType propertiesType) {
            this.propertiesType = propertiesType;
            return this;
        }

        public TransformationConfiguration build() {
            return new TransformationConfiguration(this);
        }
    }
}

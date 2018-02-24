package cz.cvut.kbss.jopa.owl2java;

public class TransformationConfiguration {

    private final String context;

    private final String packageName;

    private final String targetDir;

    private final boolean generateOwlapiIris;

    private TransformationConfiguration(TransformationConfigurationBuilder builder) {
        this.context = builder.context;
        this.packageName = builder.packageName;
        this.targetDir = builder.targetDir;
        this.generateOwlapiIris = builder.owlapiIris;
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

    public static TransformationConfigurationBuilder builder() {
        return new TransformationConfigurationBuilder();
    }

    public static class TransformationConfigurationBuilder {
        private String context;
        private String packageName = "generated";
        private String targetDir = "";
        private boolean owlapiIris;

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

        public TransformationConfiguration build() {
            return new TransformationConfiguration(this);
        }
    }
}

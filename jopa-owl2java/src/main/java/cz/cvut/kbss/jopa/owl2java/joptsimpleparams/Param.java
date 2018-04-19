package cz.cvut.kbss.jopa.owl2java.joptsimpleparams;

public enum Param {
    MAPPING_FILE("m", "mapping file"), CONTEXT("c", "context name"), WITH_IRIS("w", "with OWLAPI IRIs"), TARGET_DIR(
            "d", "output directory"), PACKAGE("p", "package"), WHOLE_ONTOLOGY_AS_IC("i",
            "interpret whole ontology as integrity constraints; this option supersedes the '-c' option."),
    IGNORE_FAILED_IMPORTS("f", "ignore failed ontology imports"),
    JAVA_CLASSNAME_ANNOTATION("jca", "java class name annotation"),
    PROPERTIES_TYPE("pt", "type of the @Properties map value set - String (default), Object");

    public final String arg;
    final String description;

    Param(String arg, String description) {
        this.arg = arg;
        this.description = description;
    }
}

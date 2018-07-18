package cz.cvut.kbss.jopa.owl2java.cli;

import cz.cvut.kbss.jopa.owl2java.config.Defaults;
import joptsimple.OptionParser;

import static cz.cvut.kbss.jopa.owl2java.cli.Option.*;

public final class CommandParserProvider {

    private CommandParserProvider() {
        throw new AssertionError();
    }

    public static OptionParser getCommandHelp() {
        return new OptionParser() {
            {
                // no options
            }
        };
    }

    public static OptionParser getCommandList() {
        final ParamOptionParser p = new ParamOptionParser();
        p.accepts(MAPPING_FILE).withRequiredArg().ofType(String.class);
        return p;
    }

    public static OptionParser getCommandTransform() {
        final ParamOptionParser p = new ParamOptionParser();
        p.accepts(MAPPING_FILE).withRequiredArg().ofType(String.class);
        p.accepts(PACKAGE).withRequiredArg().ofType(String.class).defaultsTo(Defaults.PACKAGE);
        p.accepts(CONTEXT).withOptionalArg().ofType(String.class);
        p.accepts(WITH_IRIS).withRequiredArg().ofType(Boolean.class).defaultsTo(Defaults.WITH_IRIS);
        p.accepts(TARGET_DIR).withRequiredArg().ofType(String.class).defaultsTo(Defaults.TARGET_DIR);
        p.accepts(WHOLE_ONTOLOGY_AS_IC).withOptionalArg().ofType(Boolean.class)
         .defaultsTo(Defaults.WHOLE_ONTOLOGY_AS_IC);
        p.accepts(IGNORE_FAILED_IMPORTS).withOptionalArg().ofType(Boolean.class)
         .defaultsTo(Defaults.IGNORE_FAILED_IMPORTS);
        p.accepts(JAVA_CLASSNAME_ANNOTATION).withOptionalArg().ofType(String.class)
         .defaultsTo(Defaults.JAVA_CLASSNAME_ANNOTATION);
        p.accepts(PROPERTIES_TYPE).withRequiredArg().ofType(String.class).defaultsTo(Defaults.PROPERTIES_TYPE);
        p.accepts(GENERATE_JAVADOC_FROM_COMMENT).withOptionalArg().ofType(Boolean.class)
         .defaultsTo(Defaults.GENERATE_JAVADOC_FROM_COMMENT);
        return p;
    }

    public static OptionParser getCommandVersion() {
        return new OptionParser() {
            {
                // no options
            }
        };
    }

    public static OptionParser getCommandVocabulary() {
        final ParamOptionParser p = new ParamOptionParser();
        p.accepts(MAPPING_FILE).withRequiredArg().ofType(String.class);
        p.accepts(PACKAGE).withRequiredArg().ofType(String.class).defaultsTo(Defaults.PACKAGE);
        p.accepts(CONTEXT).withRequiredArg().ofType(String.class);
        p.accepts(WITH_IRIS).withRequiredArg().ofType(Boolean.class).defaultsTo(Defaults.WITH_IRIS);
        p.accepts(TARGET_DIR).withRequiredArg().ofType(String.class).defaultsTo(Defaults.TARGET_DIR);
        p.accepts(WHOLE_ONTOLOGY_AS_IC).withOptionalArg().ofType(Boolean.class)
         .defaultsTo(Defaults.WHOLE_ONTOLOGY_AS_IC);
        p.accepts(IGNORE_FAILED_IMPORTS).withOptionalArg().ofType(Boolean.class)
         .defaultsTo(Defaults.IGNORE_FAILED_IMPORTS);
        p.accepts(GENERATE_JAVADOC_FROM_COMMENT).withOptionalArg().ofType(Boolean.class)
         .defaultsTo(Defaults.GENERATE_JAVADOC_FROM_COMMENT);
        return p;
    }
}

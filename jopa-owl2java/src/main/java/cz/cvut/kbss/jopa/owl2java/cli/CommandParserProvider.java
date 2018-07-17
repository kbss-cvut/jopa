package cz.cvut.kbss.jopa.owl2java.cli;

import cz.cvut.kbss.jopa.owl2java.Constants;
import joptsimple.OptionParser;

public class CommandParserProvider {
    public static final OptionParser getCommandHelp() {
        return new OptionParser() {
            {
                // no options
            }
        };
    }

    public static final OptionParser getCommandList() {
        final ParamOptionParser p = new ParamOptionParser();
        p.accepts(Option.MAPPING_FILE).withRequiredArg().ofType(String.class);
        return p;
    }

    public static final OptionParser getCommandTransform() {
        final ParamOptionParser p = new ParamOptionParser();
        p.accepts(Option.MAPPING_FILE).withRequiredArg().ofType(String.class);
        p.accepts(Option.PACKAGE).withRequiredArg().ofType(String.class).defaultsTo(Constants.DEFAULT_TARGET_PACKAGE);
        p.accepts(Option.CONTEXT).withOptionalArg().ofType(String.class);
        p.accepts(Option.WITH_IRIS).withRequiredArg().ofType(Boolean.class).defaultsTo(false);
        p.accepts(Option.TARGET_DIR).withRequiredArg().ofType(String.class).defaultsTo("");
        p.accepts(Option.WHOLE_ONTOLOGY_AS_IC).withOptionalArg().ofType(Boolean.class).defaultsTo(false);
        p.accepts(Option.IGNORE_FAILED_IMPORTS).withOptionalArg().ofType(Boolean.class).defaultsTo(false);
        p.accepts(Option.JAVA_CLASSNAME_ANNOTATION).withOptionalArg().ofType(String.class)
         .defaultsTo(Constants.P_CLASS_NAME);
        p.accepts(Option.PROPERTIES_TYPE).withRequiredArg().ofType(String.class)
         .defaultsTo(PropertiesType.string.name());
        p.accepts(Option.GENERATE_JAVADOC_FROM_COMMENT).withOptionalArg().ofType(Boolean.class).defaultsTo(true);
        return p;
    }

    public static final OptionParser getCommandVersion() {
        return new OptionParser() {
            {
                // no options
            }
        };
    }

    public static final OptionParser getCommandVocabulary() {
        final ParamOptionParser p = new ParamOptionParser();
        p.accepts(Option.MAPPING_FILE).withRequiredArg().ofType(String.class);
        p.accepts(Option.PACKAGE).withRequiredArg().ofType(String.class).defaultsTo(Constants.DEFAULT_TARGET_PACKAGE);
        p.accepts(Option.CONTEXT).withRequiredArg().ofType(String.class);
        p.accepts(Option.WITH_IRIS).withRequiredArg().ofType(Boolean.class).defaultsTo(false);
        p.accepts(Option.TARGET_DIR).withRequiredArg().ofType(String.class).defaultsTo("");
        p.accepts(Option.WHOLE_ONTOLOGY_AS_IC).withOptionalArg().ofType(Boolean.class).defaultsTo(false);
        p.accepts(Option.IGNORE_FAILED_IMPORTS).withOptionalArg().ofType(Boolean.class).defaultsTo(false);
        p.accepts(Option.GENERATE_JAVADOC_FROM_COMMENT).withOptionalArg().ofType(Boolean.class).defaultsTo(true);
        return p;
    }
}

package cz.cvut.kbss.jopa.owl2java.cli;

import cz.cvut.kbss.jopa.owl2java.Constants;
import cz.cvut.kbss.jopa.owl2java.joptsimpleparams.Param;
import cz.cvut.kbss.jopa.owl2java.joptsimpleparams.ParamOptionParser;
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
        p.accepts(Param.MAPPING_FILE).withRequiredArg().ofType(String.class);
        return p;
    }

    public static final OptionParser getCommandTransform() {
        final ParamOptionParser p = new ParamOptionParser();
        p.accepts(Param.MAPPING_FILE).withRequiredArg().ofType(String.class);
        p.accepts(Param.PACKAGE).withRequiredArg().ofType(String.class)
         .defaultsTo(Constants.DEFAULT_TARGET_PACKAGE);
        p.accepts(Param.CONTEXT).withOptionalArg().ofType(String.class);
        p.accepts(Param.WITH_IRIS).withRequiredArg().ofType(Boolean.class).defaultsTo(false);
        p.accepts(Param.TARGET_DIR).withRequiredArg().ofType(String.class).defaultsTo("");
        p.accepts(Param.WHOLE_ONTOLOGY_AS_IC).withOptionalArg().ofType(Boolean.class)
         .defaultsTo(false);
        p.accepts(Param.IGNORE_FAILED_IMPORTS).withOptionalArg().ofType(Boolean.class)
         .defaultsTo(false);
        p.accepts(Param.JAVA_CLASSNAME_ANNOTATION).withOptionalArg().ofType(String.class)
         .defaultsTo(Constants.P_CLASS_NAME);
        p.accepts(Param.PROPERTIES_TYPE).withRequiredArg().ofType(String.class)
         .defaultsTo(PropertiesType.string.name());
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
        p.accepts(Param.MAPPING_FILE).withRequiredArg().ofType(String.class);
        p.accepts(Param.PACKAGE).withRequiredArg().ofType(String.class)
         .defaultsTo(Constants.DEFAULT_TARGET_PACKAGE);
        p.accepts(Param.CONTEXT).withRequiredArg().ofType(String.class);
        p.accepts(Param.WITH_IRIS).withRequiredArg().ofType(Boolean.class).defaultsTo(false);
        p.accepts(Param.TARGET_DIR).withRequiredArg().ofType(String.class).defaultsTo("");
        p.accepts(Param.WHOLE_ONTOLOGY_AS_IC).withOptionalArg().ofType(Boolean.class)
         .defaultsTo(false);
        p.accepts(Param.IGNORE_FAILED_IMPORTS).withOptionalArg().ofType(Boolean.class)
         .defaultsTo(false);
        return p;
    }
}

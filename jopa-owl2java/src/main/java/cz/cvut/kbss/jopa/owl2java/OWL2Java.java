/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.owl2java;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import joptsimple.OptionParser;
import joptsimple.OptionSet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class OWL2Java {

    private static final Logger LOG = LoggerFactory.getLogger(OWL2Java.class);

    // CLI map
    private static final Map<Command, OptionParser> map = new HashMap<>();

    static {
        map.put(Command.help, new OptionParser() {
            {
                // no options
            }
        });
        map.put(Command.transform, new OptionParser() {
            {
                accepts(Param.MAPPING_FILE.arg, Param.MAPPING_FILE.description).withRequiredArg().ofType(String.class);
                accepts(Param.PACKAGE.arg, Param.PACKAGE.description).withRequiredArg().ofType(String.class)
                    .defaultsTo("generated");
                accepts(Param.CONTEXT.arg, Param.CONTEXT.description).withOptionalArg().ofType(String.class);
                accepts(Param.WITH_IRIS.arg, Param.WITH_IRIS.description).withRequiredArg().ofType(Boolean.class)
                    .defaultsTo(false);
                accepts(Param.TARGET_DIR.arg, Param.TARGET_DIR.description).withRequiredArg().ofType(String.class)
                    .defaultsTo("");
                accepts(Param.WHOLE_ONTOLOGY_AS_IC.arg, Param.WHOLE_ONTOLOGY_AS_IC.description).withOptionalArg().ofType(Boolean.class)
                    .defaultsTo(false);
            }
        });
        map.put(Command.vocabulary, new OptionParser() {
            {
                accepts(Param.MAPPING_FILE.arg, Param.MAPPING_FILE.description).withRequiredArg().ofType(String.class);
                accepts(Param.PACKAGE.arg, Param.PACKAGE.description).withRequiredArg().ofType(String.class)
                    .defaultsTo("generated");
                accepts(Param.CONTEXT.arg, Param.CONTEXT.description).withRequiredArg().ofType(String.class);
                accepts(Param.WITH_IRIS.arg, Param.WITH_IRIS.description).withRequiredArg().ofType(Boolean.class)
                    .defaultsTo(false);
                accepts(Param.TARGET_DIR.arg, Param.TARGET_DIR.description).withRequiredArg().ofType(String.class)
                    .defaultsTo("");
                accepts(Param.WHOLE_ONTOLOGY_AS_IC.arg, Param.WHOLE_ONTOLOGY_AS_IC.description).withOptionalArg().ofType(Boolean.class)
                    .defaultsTo(false);
            }
        });
        map.put(Command.list, new OptionParser() {
            {
                accepts(Param.MAPPING_FILE.arg, Param.MAPPING_FILE.description).withRequiredArg().ofType(String.class);
            }
        });
        map.put(Command.version, new OptionParser() {
            {
                // no options
            }
        });
    }

    private static void printHelp(Command cc) {
        switch (cc) {
            case help:
                System.out
                    .println(
                        "Help command gives hints on how to use other commands. Try 'OWL2Java help <command>' for more specific info.");
                System.out.println("");
                System.out.println("Syntax: OWL2Java help <command>");
                System.out.println("");
                break;
            case list:
                System.out.println("Lists all available IC contexts.");
                System.out.println("");
                System.out
                    .println("Syntax: OWL2Java list <ontology_iri> [ <options> ].");
                System.out.println("");
                break;
            case transform:
                System.out
                    .println("Transforms all ICs into annotated Java classes.");
                System.out.println("");
                System.out
                    .println("Syntax: OWL2Java transform <ontology_iri> [ <options> ].");
                System.out.println("");
                break;
            case vocabulary:
                System.out
                    .println("Generates vocabulary based on the ICs.");
                System.out.println("");
                System.out
                    .println("Syntax: OWL2Java vocabulary <ontology_iri> [ <options> ].");
                System.out.println("");
                break;
            case version:
                System.out.println("Prints the version of the OWL2Java tool.");
                break;
        }

        try {
            map.get(cc).printHelpOn(System.out);
        } catch (Exception e) {
            LOG.error(e.getMessage(), e);
        }
    }

    private static Command getCommandOrNull(String s) {
        try {
            return Command.valueOf(s);
        } catch (IllegalArgumentException e) {
            return null;
        }
    }

    public static void main(String[] args) {

        if (args.length == 0) {
            System.out.println("Syntax: OWL2Java <command> <args>. Run 'OWL2Java help' for more details");
            return;
        }

        final Command c;

        if ((c = getCommandOrNull(args[0])) == null) {
            System.err
                .println("Invalid command " + args[0] + ", try 'OWL2Java help' for the list of available commands");
            return;
        }

        final OptionParser op = map.get(c);
        final OptionSet os = op.parse(args);

        final OWL2JavaTransformer oj;

        switch (c) {
            case help:
                if (args.length != 1) {
                    final Command cc;
                    if ((cc = getCommandOrNull(args[1])) != null) {
                        printHelp(cc);
                    } else {
                        System.err.println("Invalid command " + args[0] + " " + args[1] +
                            ", try 'OWL2Java help' for the list of available commands.");
                        return;
                    }
                } else {
                    System.out.println("Available commands : " + Arrays.asList(Command.values()));
                }
                break;
            case list:
                if (!verifyArgumentCount(os)) {
                    break;
                }
                oj = getTransformer(os);

                System.out.println("Available contexts: " + oj.listContexts());
                break;
            case transform:
                transformOwlToJava(os);
                break;
            case vocabulary:
                generateVocabulary(os);
                break;
            case version:
                System.out.println("OWL2Java version " + Constants.VERSION);
                break;
            default:
                System.err.println("Unknown command '" + args[0] + "', try 'OWL2Java help.'");
        }
    }

    private static OWL2JavaTransformer getTransformer(OptionSet os) {
        OWL2JavaTransformer oj;
        oj = new OWL2JavaTransformer();
        if (os.has(Param.MAPPING_FILE.arg)) {
            oj.setOntology(os.nonOptionArguments().get(1), os.valueOf(Param.MAPPING_FILE.arg).toString(), true);
        } else {
            oj.setOntology(os.nonOptionArguments().get(1), null, true);
        }
        return oj;
    }

    private static boolean verifyArgumentCount(OptionSet os) {
        if (os.nonOptionArguments().size() != 2) {
            System.err
                .println("Exactly one ontology IRI has to be specified, got "
                    + (os.nonOptionArguments().size() - 1)
                    + ", try 'OWL2Java help' for the list of available commands");
            return false;
        }
        return true;
    }

    private static void transformOwlToJava(OptionSet os) {
        boolean whole = (Boolean) os.valueOf(Param.WHOLE_ONTOLOGY_AS_IC.arg);

        if (!whole && !verifyTransformOptions(os)) {
            return;
        }

        final OWL2JavaTransformer oj = getTransformer(os);

        oj.transform(whole ? null : os.valueOf(Param.CONTEXT.arg).toString(),
            os.valueOf(Param.PACKAGE.arg).toString(), os.valueOf(Param.TARGET_DIR.arg).toString(),
            (Boolean) os.valueOf(Param.WITH_IRIS.arg));
    }

    private static boolean verifyTransformOptions(OptionSet os) {
        if (!verifyArgumentCount(os)) {
            return false;
        }

        if (!os.has(Param.CONTEXT.arg)) {
            System.err.println("The parameter '-" + Param.CONTEXT.arg +
                "' is obligatory. Try the 'help' command for more details.");
            return false;
        }
        return true;
    }

    private static void generateVocabulary(OptionSet os) {
        boolean whole = (Boolean) os.valueOf(Param.WHOLE_ONTOLOGY_AS_IC.arg);
        if (!whole && !verifyTransformOptions(os)) {
            return;
        }
        final OWL2JavaTransformer transformer = getTransformer(os);


        transformer.generateVocabulary(whole ? null : os.valueOf(Param.CONTEXT.arg).toString(), os.valueOf(Param.PACKAGE.arg).toString(),
            os.valueOf(Param.TARGET_DIR.arg).toString(), (Boolean) os.valueOf(Param.WITH_IRIS.arg));
    }

    private enum Command {
        help, list, transform, vocabulary, version
    }

    private enum Param {
        MAPPING_FILE("m", "mapping file"), CONTEXT("c", "context name"), WITH_IRIS("w", "with OWLAPI IRIs"), TARGET_DIR(
            "d", "output directory"), PACKAGE("p", "package"), WHOLE_ONTOLOGY_AS_IC("i", "interpret whole ontology as integrity constraints; this option supersedes the '-c' option.");

        private final String arg;
        private final String description;

        Param(String arg, String description) {
            this.arg = arg;
            this.description = description;
        }
    }
}

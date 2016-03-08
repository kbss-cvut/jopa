/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.owl2java;

import joptsimple.OptionParser;
import joptsimple.OptionSet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

public class OWL2Java {

    private static final Logger LOG = LoggerFactory.getLogger(OWL2Java.class);

    public static final String VERSION = "0.7.2";

    // CLI map
    private static final Map<COMMAND, OptionParser> map = new HashMap<>();

    static {
        map.put(COMMAND.help, new OptionParser() {
            {
                // no options
            }
        });
        map.put(COMMAND.transform, new OptionParser() {
            {
                accepts("m", "mapping file").withRequiredArg().ofType(
                        String.class);
                accepts("p", "package").withRequiredArg().ofType(String.class)
                                       .defaultsTo("generated");
                accepts("c", "context name").withRequiredArg().ofType(
                        String.class);
                accepts("w", "with owlapi IRIs").withRequiredArg().ofType(
                        Boolean.class).defaultsTo(false);
                accepts("d", "output directory").withRequiredArg()
                                                .ofType(String.class).defaultsTo("");
            }
        });
        map.put(COMMAND.list, new OptionParser() {
            {
                accepts("m", "mapping file").withRequiredArg().ofType(
                        String.class);
                accepts("p", "package").withRequiredArg().ofType(String.class)
                                       .defaultsTo("generated");
            }
        });
        map.put(COMMAND.version, new OptionParser() {
            {
                // no options
            }
        });
    }

    private enum COMMAND {
        help, list, transform, version
    }

    private static void printHelp(COMMAND cc) {
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

    private static COMMAND getCommandOrNull(String s) {
        try {
            return COMMAND.valueOf(s);
        } catch (IllegalArgumentException e) {
            return null;
        }
    }

    public static void main(String[] args) {

        if (args.length == 0) {
            System.out
                    .println("Syntax: OWL2Java <command> <args>. Run 'OWL2Java help' for more details");
            return;
        }

        final COMMAND c;

        if ((c = getCommandOrNull(args[0])) == null) {
            System.out
                    .println("Invalid command "
                            + args[0]
                            + ", try 'OWL2Java help' for the list of available commands");
            return;
        }

        final OptionParser op = map.get(c);
        final OptionSet os = op.parse(args);

        final OWL2JavaTransformer oj;

        switch (c) {
            case help:
                if (args.length != 1) {
                    final COMMAND cc;
                    if ((cc = getCommandOrNull(args[1])) != null) {
                        printHelp(cc);
                    } else {
                        System.out
                                .println("Invalid command "
                                        + args[0]
                                        + " "
                                        + args[1]
                                        + ", try 'OWL2Java help' for the list of available commands");
                        return;
                    }
                } else {
                    System.out.println("Available commands : "
                            + Arrays.asList(COMMAND.values()));
                }

                break;
            case list:
                oj = new OWL2JavaTransformer();
                if (os.nonOptionArguments().size() != 2) {
                    System.out
                            .println("Exactly one ontology IRI has to be specified, got "
                                    + (os.nonOptionArguments().size() - 1)
                                    + ", try 'OWL2Java help' for the list of available commands");
                    return;
                }

                if (os.has("m")) {
                    oj.setOntology(os.nonOptionArguments().get(1), os.valueOf("m")
                                                                     .toString(), true);
                } else {
                    oj.setOntology(os.nonOptionArguments().get(1), null, true);
                }

                LOG.info("Available contexts: " + oj.listContexts());
                break;
            case transform:
                if (os.nonOptionArguments().size() != 2) {
                    System.out
                            .println("Exactly one ontology IRI has to be specified, got "
                                    + (os.nonOptionArguments().size() - 1)
                                    + ", try 'OWL2Java help' for the list of available commands");
                    return;
                }

                if (!os.has("c")) {
                    LOG.error("The parameter '-c' is obligatory. Try the 'help' command for more details.");
                    break;
                }

                oj = new OWL2JavaTransformer();
                if (os.has("m")) {
                    oj.setOntology(os.nonOptionArguments().get(1), os.valueOf("m")
                                                                     .toString(), true);
                } else {
                    oj.setOntology(os.nonOptionArguments().get(1), null, true);
                }

                if (!oj.listContexts().contains(os.valueOf("c"))) {
                    LOG.error("The parameter '-c' is invalid. Found contexts: {}", oj.listContexts());
                    break;
                }

                oj.transform(os.valueOf("c").toString(),
                        os.valueOf("p").toString(), os.valueOf("d").toString(), (Boolean) os.valueOf("w"));

                break;
            case version:
                System.out.println("OWL2Java version " + VERSION);
                break;
            default:
                System.out.println("Unknown command '" + args[0]
                        + "', try 'OWL2Java help.'");
        }
    }
}

/**
 * Copyright (C) 2019 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.owl2java.cli.CliParams;
import cz.cvut.kbss.jopa.owl2java.cli.Command;
import cz.cvut.kbss.jopa.owl2java.cli.Option;
import cz.cvut.kbss.jopa.owl2java.config.TransformationConfiguration;
import joptsimple.OptionParser;
import joptsimple.OptionSet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Optional;

public class OWL2Java {

    private static final Logger LOG = LoggerFactory.getLogger(OWL2Java.class);

    private static void printHelp(Command cc) {
        final PrintStream os = System.out;

        os.print(cc.helpText);
        try {
            cc.parser.printHelpOn(os);
        } catch (Exception e) {
            LOG.error(e.getMessage(), e);
        }
    }

    private static Optional<Command> getCommand(String s) {
        try {
            return Optional.of(Command.valueOf(s));
        } catch (IllegalArgumentException e) {
            return Optional.empty();
        }
    }

    public static void main(String[] args) {

        if (args.length == 0) {
            System.out.println("Syntax: OWL2Java <command> <args>. Run 'OWL2Java help' for more details");
            return;
        }

        final Optional<Command> c = getCommand(args[0]);

        if (!c.isPresent()) {
            System.err
                    .println("Invalid command " + args[0] + ", try 'OWL2Java help' for the list of available commands");
            return;
        }

        final OptionParser op = c.get().parser;
        final OptionSet os = op.parse(args);
        final CliParams input = new CliParams(os);

        final OWL2JavaTransformer oj;

        switch (c.get()) {
            case help:
                if (args.length != 1) {
                    final Optional<Command> cc = getCommand(args[1]);
                    if (cc.isPresent()) {
                        printHelp(cc.get());
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
                if (invalidArgumentCount(input)) {
                    break;
                }
                oj = getTransformer(input);

                System.out.println("Available contexts: " + oj.listContexts());
                break;
            case transform:
                if (invalidArgumentCount(input)) {
                    break;
                }
                transformOwlToJava(input);
                break;
            case vocabulary:
                if (invalidArgumentCount(input)) {
                    break;
                }
                generateVocabulary(input);
                break;
            case version:
                System.out.println("OWL2Java version " + Constants.VERSION);
                break;
            default:
                System.err.println("Unknown command '" + args[0] + "', try 'OWL2Java help'.");
        }
    }

    private static OWL2JavaTransformer getTransformer(CliParams input) {
        OWL2JavaTransformer oj;
        oj = new OWL2JavaTransformer();
        if (input.has(Option.MAPPING_FILE.arg)) {
            oj.setOntology(input.nonOptionArguments().get(1), input.valueOf(Option.MAPPING_FILE.arg).toString());
        } else {
            oj.setOntology(input.nonOptionArguments().get(1), null);
        }
        oj.ignoreMissingImports(input.is(Option.IGNORE_FAILED_IMPORTS.arg));
        return oj;
    }

    private static boolean invalidArgumentCount(CliParams input) {
        if (input.nonOptionArguments().size() != 2) {
            System.err
                    .println("Exactly one ontology IRI has to be specified, got "
                            + (input.nonOptionArguments().size() - 1)
                            + ", try 'OWL2Java help' for the list of available commands");
            return true;
        }
        return false;
    }

    private static void transformOwlToJava(CliParams input) {
        boolean whole = input.is(Option.WHOLE_ONTOLOGY_AS_IC.arg);

        if (!whole && invalidTransformationOptions(input)) {
            return;
        }

        final TransformationConfiguration config = TransformationConfiguration.config(input);

        final OWL2JavaTransformer transformer = getTransformer(input);

        transformer.transform(config);
    }

    private static boolean invalidTransformationOptions(CliParams input) {
        if (invalidArgumentCount(input)) {
            return true;
        }

        if (!input.has(Option.CONTEXT.arg)) {
            System.err.println("The parameter '-" + Option.CONTEXT.arg +
                    "' is obligatory. Try the 'help' command for more details.");
            return true;
        }
        return false;
    }

    private static void generateVocabulary(CliParams input) {
        final boolean whole = input.is(Option.WHOLE_ONTOLOGY_AS_IC.arg);
        if (!whole && invalidTransformationOptions(input)) {
            return;
        }
        final OWL2JavaTransformer transformer = getTransformer(input);

        final TransformationConfiguration config = TransformationConfiguration.config(input);

        transformer.generateVocabulary(config);
    }
}

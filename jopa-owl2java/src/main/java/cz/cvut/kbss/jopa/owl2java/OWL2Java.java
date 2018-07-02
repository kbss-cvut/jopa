/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.owl2java;

import cz.cvut.kbss.jopa.owl2java.cli.Command;
import cz.cvut.kbss.jopa.owl2java.cli.PropertiesType;
import cz.cvut.kbss.jopa.owl2java.joptsimpleparams.Param;
import java.io.PrintStream;
import java.util.Arrays;
import joptsimple.OptionParser;
import joptsimple.OptionSet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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

        final OptionParser op = c.parser;
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
                if (invalidArgumentCount(os)) {
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
        oj = new OWL2JavaTransformer(os);
        if (os.has(Param.MAPPING_FILE.arg)) {
            oj.setOntology(os.nonOptionArguments().get(1), os.valueOf(Param.MAPPING_FILE.arg).toString());
        } else {
            oj.setOntology(os.nonOptionArguments().get(1), null);
        }
        oj.ignoreMissingImports((Boolean) os.valueOf(Param.IGNORE_FAILED_IMPORTS.arg));
        return oj;
    }

    private static boolean invalidArgumentCount(OptionSet os) {
        if (os.nonOptionArguments().size() != 2) {
            System.err
                    .println("Exactly one ontology IRI has to be specified, got "
                            + (os.nonOptionArguments().size() - 1)
                            + ", try 'OWL2Java help' for the list of available commands");
            return true;
        }
        return false;
    }

    private static void transformOwlToJava(OptionSet os) {
        boolean whole = (Boolean) os.valueOf(Param.WHOLE_ONTOLOGY_AS_IC.arg);

        if (!whole && invalidTransformationOptions(os)) {
            return;
        }

        final TransformationConfiguration.TransformationConfigurationBuilder configBuilder =
                TransformationConfiguration.builder();
        if (!whole) {
            configBuilder.context(os.valueOf(Param.CONTEXT.arg).toString());
        }
        configBuilder.packageName(os.valueOf(Param.PACKAGE.arg).toString())
                     .targetDir(os.valueOf(Param.TARGET_DIR.arg).toString())
                     .addOwlapiIris((Boolean) os.valueOf(Param.WITH_IRIS.arg)).propertiesType(
            PropertiesType.valueOf(os.valueOf(Param.PROPERTIES_TYPE.arg).toString()));

        final OWL2JavaTransformer transformer = getTransformer(os);

        transformer.transform(configBuilder.build());
    }

    private static boolean invalidTransformationOptions(OptionSet os) {
        if (invalidArgumentCount(os)) {
            return true;
        }

        if (!os.has(Param.CONTEXT.arg)) {
            System.err.println("The parameter '-" + Param.CONTEXT.arg +
                    "' is obligatory. Try the 'help' command for more details.");
            return true;
        }
        return false;
    }

    private static void generateVocabulary(OptionSet os) {
        boolean whole = (Boolean) os.valueOf(Param.WHOLE_ONTOLOGY_AS_IC.arg);
        if (!whole && invalidTransformationOptions(os)) {
            return;
        }
        final OWL2JavaTransformer transformer = getTransformer(os);

        final TransformationConfiguration.TransformationConfigurationBuilder builder =
                TransformationConfiguration.builder();
        if (!whole) {
            builder.context(os.valueOf(Param.CONTEXT.arg).toString());
        }
        final TransformationConfiguration config = builder.packageName(os.valueOf(Param.PACKAGE.arg).toString())
                                                          .targetDir(os.valueOf(Param.TARGET_DIR.arg).toString())
                                                          .addOwlapiIris((Boolean) os.valueOf(Param.WITH_IRIS.arg))
                                                          .build();


        transformer.generateVocabulary(config);
    }
}

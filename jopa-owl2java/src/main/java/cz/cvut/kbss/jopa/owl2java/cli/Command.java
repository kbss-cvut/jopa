package cz.cvut.kbss.jopa.owl2java.cli;

import joptsimple.OptionParser;

public enum Command {
    help(CommandParserProvider.getCommandHelp(), String.join(System.getProperty("line.separator"),
        "Help command gives hints on how to use other commands. Try 'OWL2Java help <command>' for"
        + " more specific info.",
        "", "Syntax: OWL2Java help <command>", "")),

    list(CommandParserProvider.getCommandList(), String
        .join(System.getProperty("line.separator"), "Lists all available IC contexts.", "",
            "Syntax: OWL2Java list <ontology_iri> [ <options> ].", "")),

    transform(CommandParserProvider.getCommandTransform(), String
        .join(System.getProperty("line.separator"),
            "Transforms all ICs into annotated Java classes.", "",
            "Syntax: OWL2Java transform <ontology_iri> [ <options> ].", "")),

    vocabulary(CommandParserProvider.getCommandVocabulary(), String
        .join(System.getProperty("line.separator"), "Generates vocabulary based on the ICs.", "",
            "Syntax: OWL2Java vocabulary <ontology_iri> [ <options> ].", "")),

    version(CommandParserProvider.getCommandVersion(), "Prints the version of the OWL2Java tool.");

    public final OptionParser parser;
    public final String helpText;

    Command(OptionParser parser, String help) {
        this.parser = parser;
        this.helpText = help;
    }
}

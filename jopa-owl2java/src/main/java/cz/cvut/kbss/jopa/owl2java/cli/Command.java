/**
 * Copyright (C) 2020 Czech Technical University in Prague
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

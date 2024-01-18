/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.owl2java.cli;

import cz.cvut.kbss.jopa.owl2java.config.Defaults;
import joptsimple.OptionParser;

import static cz.cvut.kbss.jopa.owl2java.cli.Option.*;

public final class CommandParserProvider {

    private CommandParserProvider() {
        throw new AssertionError();
    }

    public static OptionParser getCommandHelp() {
        return new OptionParser();
    }

    public static OptionParser getCommandList() {
        final ParamOptionParser p = new ParamOptionParser();
        p.accepts(MAPPING_FILE).withRequiredArg().ofType(String.class);
        return p;
    }

    public static OptionParser getCommandTransform() {
        ParamOptionParser p = initParserWithCommonOptions();
        p.accepts(JAVA_CLASSNAME_ANNOTATION).withRequiredArg().ofType(String.class)
                .defaultsTo(Defaults.JAVA_CLASSNAME_ANNOTATION);
        p.accepts(PROPERTIES_TYPE).withRequiredArg().ofType(String.class).defaultsTo(Defaults.PROPERTIES_TYPE);
        return p;
    }

    private static ParamOptionParser initParserWithCommonOptions() {
        final ParamOptionParser p = new ParamOptionParser();
        p.accepts(MAPPING_FILE).withRequiredArg().ofType(String.class);
        p.accepts(PACKAGE).withRequiredArg().ofType(String.class).defaultsTo(Defaults.PACKAGE);
        p.accepts(CONTEXT).withRequiredArg().ofType(String.class);
        p.accepts(WITH_IRIS).withOptionalArg().ofType(Boolean.class).defaultsTo(true);
        p.accepts(TARGET_DIR).withRequiredArg().ofType(String.class).defaultsTo(Defaults.TARGET_DIR);
        p.accepts(WHOLE_ONTOLOGY_AS_IC).withOptionalArg().ofType(Boolean.class).defaultsTo(true);
        p.accepts(IGNORE_FAILED_IMPORTS).withOptionalArg().ofType(Boolean.class).defaultsTo(true);
        p.accepts(GENERATE_JAVADOC_FROM_COMMENT).withOptionalArg().ofType(Boolean.class).defaultsTo(true);
        p.accepts(ONTOLOGY_PREFIX_PROPERTY).withOptionalArg().ofType(String.class).defaultsTo(Defaults.ONTOLOGY_PREFIX_PROPERTY);
        return p;
    }

    public static OptionParser getCommandVersion() {
        return new OptionParser();
    }

    public static OptionParser getCommandVocabulary() {
        return initParserWithCommonOptions();
    }
}

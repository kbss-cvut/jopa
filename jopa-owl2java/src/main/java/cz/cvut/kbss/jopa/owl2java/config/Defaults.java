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
package cz.cvut.kbss.jopa.owl2java.config;

import cz.cvut.kbss.jopa.owl2java.Constants;
import cz.cvut.kbss.jopa.owl2java.cli.Option;
import cz.cvut.kbss.jopa.owl2java.cli.PropertiesType;

/**
 * Default values of {@link Option}s.
 */
public class Defaults {

    /**
     * @see Option#WITH_IRIS
     */
    public static final boolean WITH_IRIS = false;

    /**
     * @see Option#TARGET_DIR
     */
    public static final String TARGET_DIR = "";

    /**
     * @see Option#PACKAGE
     */
    public static final String PACKAGE = "generated";

    /**
     * @see Option#WHOLE_ONTOLOGY_AS_IC
     */
    public static final boolean WHOLE_ONTOLOGY_AS_IC = false;

    /**
     * @see Option#IGNORE_FAILED_IMPORTS
     */
    public static final boolean IGNORE_FAILED_IMPORTS = false;

    /**
     * @see Option#PREFER_MULTILINGUAL_STRINGS
     */
    public static final boolean PREFER_MULTILINGUAL_STRINGS = true;

    /**
     * @see Option#JAVA_CLASSNAME_ANNOTATION
     */
    public static final String JAVA_CLASSNAME_ANNOTATION = Constants.P_CLASS_NAME;

    /**
     * @see Option#PROPERTIES_TYPE
     */
    public static final String PROPERTIES_TYPE = PropertiesType.string.name();

    /**
     * @see Option#GENERATE_JAVADOC_FROM_COMMENT
     */
    public static final boolean GENERATE_JAVADOC_FROM_COMMENT = true;

    /**
     * @see Option#GENERATE_ANNOTATION_FIELDS
     */
    public static final boolean GENERATE_ANNOTATION_FIELDS = true;

    /**
     * @see Option#GENERATE_THING
     */
    public static final boolean GENERATE_THING = true;

    /**
     * @see Option#ONTOLOGY_PREFIX_PROPERTY
     */
    public static final String ONTOLOGY_PREFIX_PROPERTY = "http://purl.org/vocab/vann/preferredNamespacePrefix";

    /**
     * @see Option#ALWAYS_USE_ONTOLOGY_PREFIX
     */
    public static final boolean USE_ONTOLOGY_PREFIX = false;

    private Defaults() {
        throw new AssertionError();
    }
}

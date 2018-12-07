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
package cz.cvut.kbss.jopa;

/**
 * @deprecated Use {@link cz.cvut.kbss.jopa.vocabulary.OWL}, {@link cz.cvut.kbss.jopa.vocabulary.RDF}, {@link
 * cz.cvut.kbss.jopa.vocabulary.RDFS}, {@link cz.cvut.kbss.jopa.vocabulary.XSD}, and {@link cz.cvut.kbss.jopa.vocabulary.DC}
 */
@Deprecated
public class CommonVocabulary {

    public static final String RDF_NAMESPACE = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    public static final String RDFS_NAMESPACE = "http://www.w3.org/2000/01/rdf-schema#";
    public static final String XSD_NAMESPACE = "http://www.w3.org/2001/XMLSchema#";

    public static final String RDFS_LABEL = RDFS_NAMESPACE + "label";

    public static final String RDFS_COMMENT = RDFS_NAMESPACE + "comment";

    public static final String DC_DESCRIPTION = "http://purl.org/dc/elements/1.1/description";

    public static final String RDF_TYPE = RDF_NAMESPACE + "type";

    public static final String RDF_VALUE = RDF_NAMESPACE + "value";

    /**
     * Boolean XML Schema data type.
     */
    public static final String XSD_BOOLEAN = XSD_NAMESPACE + "boolean";

    /**
     * Short XML Schema data type.
     */
    public static final String XSD_SHORT = XSD_NAMESPACE + "short";

    /**
     * Integer XML Schema data type.
     */
    public static final String XSD_INT = XSD_NAMESPACE + "int";

    /**
     * Long XML Schema data type.
     */
    public static final String XSD_LONG = XSD_NAMESPACE + "long";

    /**
     * Double XML Schema data type.
     */
    public static final String XSD_DOUBLE = XSD_NAMESPACE + "double";

    /**
     * Float XML Schema data type.
     */
    public static final String XSD_FLOAT = XSD_NAMESPACE + "float";

    /**
     * Date XML Schema data type.
     */
    public static final String XSD_DATETIME = XSD_NAMESPACE + "dateTime";

    private CommonVocabulary() {
        throw new AssertionError();
    }
}

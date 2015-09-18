/**
 * Copyright (C) 2011 Czech Technical University in Prague
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

public class CommonVocabulary {

    private CommonVocabulary() {
        throw new AssertionError();
    }

    public static final String RDFS_LABEL = "http://www.w3.org/2000/01/rdf-schema#label";

    public static final String DC_DESCRIPTION = "http://purl.org/dc/elements/1.1/description";

    public static final String RDF_TYPE = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";

    /**
     * Boolean XML Schema data type.
     */
    public static final String XSD_BOOLEAN = "http://www.w3.org/2001/XMLSchema#boolean";

    /**
     * Short XML Schema data type.
     */
    public static final String XSD_SHORT = "http://www.w3.org/2001/XMLSchema#short";

    /**
     * Integer XML Schema data type.
     */
    public static final String XSD_INTEGER = "http://www.w3.org/2001/XMLSchema#integer";

    /**
     * Long XML Schema data type.
     */
    public static final String XSD_LONG = "http://www.w3.org/2001/XMLSchema#long";

    /**
     * Double XML Schema data type.
     */
    public static final String XSD_DOUBLE = "http://www.w3.org/2001/XMLSchema#double";

    /**
     * Float XML Schema data type.
     */
    public static final String XSD_FLOAT = "http://www.w3.org/2001/XMLSchema#float";

    /**
     * Date XML Schema data type.
     */
    public static final String XSD_DATETIME = "http://www.w3.org/2001/XMLSchema#dateTime";
}

/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.Literal;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;
import java.util.Objects;

/**
 * Converter for working with RDF literals with explicit datatype mapping ({@link cz.cvut.kbss.ontodriver.model.Literal}).
 * <p>
 * The attribute value then represents the lexical form of the literal and should be a {@code String}.
 * <p>
 * Note that the conversion to attribute value does not perform any validation regarding match between the value and the specified datatype IRI.
 * So, for example, a value read from the repository as {@code Integer} will be mapped by this converter to an attribute with
 * datatype {@code xsd:boolean} without any exceptions.
 */
public class ToRdfLiteralConverter implements ConverterWrapper<String, Object> {

    private final String datatype;

    public ToRdfLiteralConverter(String datatype) {
        this.datatype = Objects.requireNonNull(datatype);
    }

    @Override
    public Object convertToAxiomValue(String value) {
        return value != null ? new Literal(value, datatype) : null;
    }

    @Override
    public String convertToAttribute(Object value) {
        if (value instanceof Literal) {
            return ((Literal) value).getLexicalForm();
        }
        if (value instanceof LangString) {
            return ((LangString) value).getValue();
        }
        return value.toString();
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return !NamedResource.class.isAssignableFrom(type) && !URI.class.equals(type);
    }
}

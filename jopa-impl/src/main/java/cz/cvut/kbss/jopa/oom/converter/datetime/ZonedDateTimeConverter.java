/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.oom.converter.datetime;

import cz.cvut.kbss.jopa.datatype.xsd.XsdDateTimeMapper;
import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;
import cz.cvut.kbss.jopa.vocabulary.XSD;
import cz.cvut.kbss.ontodriver.model.Literal;

import java.time.OffsetDateTime;
import java.time.ZonedDateTime;

/**
 * Converts between Java 8 {@link ZonedDateTime} and a supported xsd:dateTime representation.
 * <p>
 * This converter supports {@link OffsetDateTime} and {@link cz.cvut.kbss.ontodriver.model.Literal} as date time.
 */
public class ZonedDateTimeConverter implements ConverterWrapper<ZonedDateTime, Object> {

    @Override
    public Object convertToAxiomValue(ZonedDateTime value) {
        assert value != null;
        return value.toOffsetDateTime();
    }

    @Override
    public ZonedDateTime convertToAttribute(Object value) {
        assert value != null;
        if (value instanceof OffsetDateTime) {
            return ((OffsetDateTime) value).toZonedDateTime();
        } else {
            assert value instanceof Literal;
            final Literal literal = (Literal) value;
            assert XSD.DATETIME.equals(literal.getDatatype());
            return (XsdDateTimeMapper.map(literal.getLexicalForm())).toZonedDateTime();
        }
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return OffsetDateTime.class.isAssignableFrom(type) || Literal.class.isAssignableFrom(type);
    }
}

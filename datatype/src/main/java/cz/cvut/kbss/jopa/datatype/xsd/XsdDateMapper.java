/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.datatype.xsd;

import cz.cvut.kbss.jopa.datatype.exception.DatatypeMappingException;

import javax.xml.datatype.XMLGregorianCalendar;
import java.time.LocalDate;

/**
 * Maps values of {@link cz.cvut.kbss.jopa.vocabulary.XSD#DATE} to Java {@link LocalDate}.
 * <p>
 * Note that this mapping loses timezone information, if it were present in the literal.
 */
public class XsdDateMapper {

    private XsdDateMapper() {
        throw new AssertionError();
    }

    /**
     * Maps the specified value to {@link LocalDate}.
     *
     * @param value Value to map
     * @return Parsed local date
     */
    public static LocalDate map(String value) {
        try {
            final XMLGregorianCalendar cal = DatatypeFactoryProvider.getFactory().newXMLGregorianCalendar(value);
            return LocalDate.of(cal.getYear(), cal.getMonth(), cal.getDay());
        } catch (IllegalArgumentException e) {
            throw new DatatypeMappingException("Invalid value provided as xsd:date.", e);
        }
    }
}

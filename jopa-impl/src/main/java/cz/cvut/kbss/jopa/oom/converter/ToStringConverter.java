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

public class ToStringConverter implements ConverterWrapper<String, Object> {

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return true;
    }

    @Override
    public Object convertToAxiomValue(String value) {
        return value;
    }

    @Override
    public String convertToAttribute(Object value) {
        return value instanceof LangString ? ((LangString) value).getValue() : value.toString();
    }
}

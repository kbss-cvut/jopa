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
package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.NamedResource;

/**
 * Converts literal lexical form to Java {@code String}.
 * <p>
 * This converter ensures seamless support for lexical forms.
 */
public class ToLexicalFormConverter implements ConverterWrapper<String, Object> {

    public static final ToLexicalFormConverter INSTANCE = new ToLexicalFormConverter();

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        // Anything but a NamedResource (i.e., a literal since anonymous individuals are not supported) can be transformed to lexical form
        return !NamedResource.class.isAssignableFrom(type);
    }

    @Override
    public Object convertToAxiomValue(String value) {
        return value;
    }

    @Override
    public String convertToAttribute(Object value) {
        assert value != null;
        return value instanceof LangString ? ((LangString) value).getValue() : value.toString();
    }
}

/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.sesame;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

public final class TestUtils {

	private TestUtils() {
	}

	public static void setMock(String fieldName, Class<?> cls, Object mock) throws Exception {
		final Field field = cls.getDeclaredField(fieldName);
		setFieldValue(null, mock, field);
	}

	private static void setFieldValue(Object target, Object mock, final Field field)
			throws Exception {
		field.setAccessible(true);
		removeFinalModifier(field);
		field.set(target, mock);
	}

	public static void setMock(String fieldName, Object target, Object mock) throws Exception {
		final Field field = target.getClass().getDeclaredField(fieldName);
		setFieldValue(target, mock, field);
	}

	private static void removeFinalModifier(Field field) throws Exception {
		Field modifiersField = Field.class.getDeclaredField("modifiers");
		modifiersField.setAccessible(true);
		modifiersField.setInt(field, field.getModifiers() & ~Modifier.FINAL);
	}
}

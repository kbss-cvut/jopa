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
			throws Exception, IllegalAccessException {
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

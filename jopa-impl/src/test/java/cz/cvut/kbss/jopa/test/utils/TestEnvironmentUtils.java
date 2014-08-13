package cz.cvut.kbss.jopa.test.utils;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.net.URI;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.Set;

import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSetImpl;

public final class TestEnvironmentUtils {

	private static Random random;

	private TestEnvironmentUtils() {
		throw new AssertionError();
	}

	static {
		random = new Random();
	}

	public static ObjectChangeSet createObjectChangeSet(Object original, Object clone, URI context) {
		return new ObjectChangeSetImpl(original, clone, context);
	}

	public static int randomInt(int max) {
		return random.nextInt(max);
	}

	public static boolean arePropertiesEqual(Map<String, Set<String>> pOne,
			Map<String, Set<String>> pTwo) {
		if (pOne.size() != pTwo.size()) {
			return false;
		}
		for (Entry<String, Set<String>> e : pOne.entrySet()) {
			if (!pTwo.containsKey(e.getKey())) {
				return false;
			}
			final Set<String> set = pTwo.get(e.getKey());
			if (!e.getValue().equals(set)) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Sets value of field {@code field} on the {@code target} object to the
	 * specified {@code mock}. </p>
	 * 
	 * This method also works for final fields. Note that in case static fields
	 * are set, it is the responsibility of the client to reset the field to the
	 * original value in test cleanup.
	 * 
	 * @param target
	 *            Target instance, can be {@code null} for static fields
	 * @param field
	 *            Field to set
	 * @param mock
	 *            The new value
	 */
	public static void setMock(Object target, Field field, Object mock) throws Exception {
		assert field != null;
		field.setAccessible(true);
		removeFinalModifier(field);
		field.set(target, mock);
	}

	private static void removeFinalModifier(Field field) throws Exception {
		// remove final modifier from field
		Field modifiersField = Field.class.getDeclaredField("modifiers");
		modifiersField.setAccessible(true);
		modifiersField.setInt(field, field.getModifiers() & ~Modifier.FINAL);
	}
}

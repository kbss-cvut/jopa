package cz.cvut.kbss.jopa.test.utils;

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
}

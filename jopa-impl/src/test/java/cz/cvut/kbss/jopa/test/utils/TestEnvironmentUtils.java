package cz.cvut.kbss.jopa.test.utils;

import java.net.URI;
import java.util.Random;

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
}

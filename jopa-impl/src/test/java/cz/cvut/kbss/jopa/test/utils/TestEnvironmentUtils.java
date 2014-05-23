package cz.cvut.kbss.jopa.test.utils;

import java.net.URI;

import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSetImpl;

public final class TestEnvironmentUtils {

	private TestEnvironmentUtils() {
		throw new AssertionError();
	}

	public static ObjectChangeSet createObjectChangeSet(Object original, Object clone, URI context) {
		return new ObjectChangeSetImpl(original, clone, context);
	}
}

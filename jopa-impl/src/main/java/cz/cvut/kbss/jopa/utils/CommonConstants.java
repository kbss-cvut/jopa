package cz.cvut.kbss.jopa.utils;

import java.net.URI;

import cz.cvut.kbss.jopa.model.annotations.Properties;

public final class CommonConstants {

	private CommonConstants() {
		throw new AssertionError();
	}

	/**
	 * Placeholder for unknown URI in {@link Properties} assertions.
	 */
	public static final URI PROPERTIES_URI = URI.create("http://jopa.org");
}

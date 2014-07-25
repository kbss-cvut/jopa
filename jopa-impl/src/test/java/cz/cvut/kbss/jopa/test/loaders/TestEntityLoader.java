package cz.cvut.kbss.jopa.test.loaders;

import java.util.logging.Logger;

import org.junit.Test;

import cz.cvut.kbss.jopa.loaders.EntityLoader;

public class TestEntityLoader {

	private static final Logger LOG = Logger.getLogger(TestEntityLoader.class.getName());

	@Test
	public void testEntityLoading() {
		LOG.config("Test: entity loading.");
        EntityLoader.discoverEntityClasses();
	}

}

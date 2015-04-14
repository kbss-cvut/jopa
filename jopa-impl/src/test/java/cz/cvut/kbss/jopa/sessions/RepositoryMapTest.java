package cz.cvut.kbss.jopa.sessions;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Map;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassE;

public class RepositoryMapTest {

	private static final URI CONTEXT = URI.create("http://repositoryMapTestContext");

	private static OWLClassA entityA;
	private static OWLClassB entityB;
	private static OWLClassE entityE;
	private static Descriptor descriptor;

	private RepositoryMap repoMap;

	private Map<URI, Map<Object, Object>> theMap;
	private Map<Object, Descriptor> descriptors;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		entityA = new OWLClassA();
		entityA.setUri(URI.create("http://krizik.felk.cvut.cz/jopa/ontology/entityA"));
		entityA.setStringAttribute("SomeString");
		entityB = new OWLClassB();
		entityB.setUri(URI.create("http://krizik.felk.cvut.cz/jopa/ontology/entityB"));
		entityB.setStringAttribute("otherString");
		entityE = new OWLClassE();
		entityE.setUri(URI.create("http://krizik.felk.cvut.cz/jopa/ontology/entityE"));
		descriptor = new EntityDescriptor(CONTEXT);
	}

	@SuppressWarnings("unchecked")
	@Before
	public void setUp() throws Exception {
		this.repoMap = new RepositoryMap();
		repoMap.initDescriptors();
		final Field mapField = RepositoryMap.class.getDeclaredField("map");
		mapField.setAccessible(true);
		this.theMap = (Map<URI, Map<Object, Object>>) mapField.get(repoMap);
		final Field descriptorsField = RepositoryMap.class.getDeclaredField("entityDescriptors");
		descriptorsField.setAccessible(true);
		this.descriptors = (Map<Object, Descriptor>) descriptorsField.get(repoMap);
	}

	@Test
	public void testAdd() {
		repoMap.add(descriptor, entityA.getUri(), entityA);
		assertTrue(repoMap.contains(descriptor, entityA.getUri()));
		assertTrue(theMap.containsKey(CONTEXT));
		assertTrue(theMap.get(CONTEXT).containsKey(entityA.getUri()));
	}

	@Test
	public void testRemove() {
		repoMap.add(descriptor, entityA.getUri(), entityA);
		repoMap.add(descriptor, entityB.getUri(), entityB);
		assertTrue(repoMap.contains(descriptor, entityA.getUri()));
		assertTrue(repoMap.contains(descriptor, entityB.getUri()));

		repoMap.remove(descriptor, entityB.getUri());
		assertTrue(repoMap.contains(descriptor, entityA.getUri()));
		assertFalse(repoMap.contains(descriptor, entityB.getUri()));
	}

	@Test
	public void testGet() {
		repoMap.add(descriptor, entityA.getUri(), entityA);
		repoMap.add(descriptor, entityB.getUri(), entityB);

		final Object res = repoMap.get(descriptor, entityA.getUri());
		assertNotNull(res);
		assertSame(entityA, res);
	}

	@Test
	public void testGetUnknown() {
		final Object res = repoMap.get(descriptor, entityA.getUri());
		assertNull(res);
	}

	@Test
	public void testContains() {
		repoMap.add(descriptor, entityA.getUri(), entityA);
		repoMap.add(descriptor, entityB.getUri(), entityB);

		assertTrue(repoMap.contains(descriptor, entityA.getUri()));
		assertTrue(repoMap.contains(descriptor, entityB.getUri()));
		assertFalse(repoMap.contains(descriptor, entityE));
		final EntityDescriptor desc = new EntityDescriptor();
		assertFalse(repoMap.contains(desc, entityA.getUri()));
	}

	@Test
	public void testAddEntityToRepository() {
		repoMap.addEntityToRepository(entityA, descriptor);
		assertTrue(descriptors.containsKey(entityA));
		assertSame(descriptor, descriptors.get(entityA));
	}

	@Test
	public void testRemoveEntityToRepository() {
		repoMap.addEntityToRepository(entityA, descriptor);
		assertTrue(descriptors.containsKey(entityA));

		repoMap.removeEntityToRepository(entityA);
		assertFalse(descriptors.containsKey(entityA));
	}

	@Test
	public void testGetEntityDescriptor() {
		repoMap.addEntityToRepository(entityA, descriptor);
		assertTrue(descriptors.containsKey(entityA));

		final Descriptor res = repoMap.getEntityDescriptor(entityA);
		assertNotNull(res);
		assertSame(descriptor, res);
	}

	@Test
	public void testClear() {
		repoMap.add(descriptor, entityA.getUri(), entityA);
		repoMap.add(descriptor, entityB.getUri(), entityB);
		repoMap.addEntityToRepository(entityA, descriptor);

		repoMap.clear();
		assertFalse(repoMap.contains(descriptor, entityA.getUri()));
		assertFalse(repoMap.contains(descriptor, entityB.getUri()));
		assertNull(repoMap.getEntityDescriptor(entityA));
	}

}

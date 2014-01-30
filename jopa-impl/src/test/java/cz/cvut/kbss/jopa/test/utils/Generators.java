package cz.cvut.kbss.jopa.test.utils;

import java.net.URI;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import cz.cvut.kbss.jopa.test.OWLClassA;

/**
 * Generators of test data.
 * 
 * @author ledvima1
 * 
 */
public abstract class Generators {

	private static final int DEFAULT_SIZE = 5;
	private static final Set<String> TYPES = getTypes();

	private Generators() {
		// Private constructor
	}

	public static List<OWLClassA> createSimpleList() {
		return createSimpleList(DEFAULT_SIZE);
	}

	public static List<OWLClassA> createReferencedList() {
		return createReferencedList(DEFAULT_SIZE);
	}

	public static List<OWLClassA> createSimpleList(int size) {
		assert size > 0;
		final List<OWLClassA> lst = new ArrayList<>(size);
		int counter = 110;
		for (int i = 0; i < size; i++) {
			final OWLClassA a = new OWLClassA();
			a.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityASimple"
					+ counter));
			a.setStringAttribute("stringAttributeeee" + counter++);
			a.setTypes(TYPES);
			lst.add(a);
		}
		return lst;
	}

	public static List<OWLClassA> createReferencedList(int size) {
		assert size > 0;
		final List<OWLClassA> lst = new ArrayList<>(size);
		int counter = 101;
		for (int i = 0; i < size; i++) {
			final OWLClassA a = new OWLClassA();
			a.setUri(URI
					.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityAReferenced"
							+ counter));
			a.setStringAttribute("stringAttributeeee" + counter++);
			a.setTypes(TYPES);
			lst.add(a);
		}
		return lst;
	}

	private static Set<String> getTypes() {
		final Set<String> types = new HashSet<>(3);
		types.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA");
		types.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassX");
		types.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassZ");
		return types;
	}
}

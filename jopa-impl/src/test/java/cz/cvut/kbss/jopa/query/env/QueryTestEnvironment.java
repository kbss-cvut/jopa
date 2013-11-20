package cz.cvut.kbss.jopa.query.env;

import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.owlapi.OWLClassA;
import cz.cvut.kbss.jopa.owlapi.OWLClassB;
import cz.cvut.kbss.jopa.owlapi.OWLClassC;
import cz.cvut.kbss.jopa.owlapi.OWLClassD;
import cz.cvut.kbss.jopa.owlapi.OWLClassE;

public final class QueryTestEnvironment {

	private static final Logger LOG = Logger.getLogger(QueryTestEnvironment.class.getName());

	private static final String BASE_A = "http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA_";
	private static final String TYPE_A = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#TypeA";
	private static final String BASE_B = "http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityB_";
	private static final String BASE_C = "http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityC_";
	private static final String BASE_D = "http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityD_";

	/**
	 * Default prefixes for SPARQL. </p>
	 * 
	 * Currently: owl, rdf, rdfs
	 */
	public static final String OWL_PREFIX = "PREFIX owl: <http://www.w3.org/2002/07/owl#>";
	public static final String RDF_PREFIX = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>";
	public static final String RDFS_PREFIX = "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>";

	private QueryTestEnvironment() {
		// Private constructor to prevent instantiation
	}

	/**
	 * Generates and persists test data into the default context of the
	 * specified entity manager. </p>
	 * 
	 * The generated data are then returned.
	 * 
	 * @param em
	 *            EntityManager
	 * @return Map of the persisted test data
	 */
	public static Map<Class<?>, List<?>> generateTestData(EntityManager em) {
		assert em != null;
		final Map<Class<?>, List<?>> map = generate();
		LOG.config("Persisting test data...");
		em.getTransaction().begin();
		try {
			for (List<?> l : map.values()) {
				for (Object o : l) {
					em.persist(o);
				}
			}
			em.getTransaction().commit();
		} catch (RuntimeException e) {
			if (em.getTransaction().isActive()) {
				em.getTransaction().rollback();
			}
			throw e;
		}
		return map;
	}

	private static Map<Class<?>, List<?>> generate() {
		LOG.config("Generating test data...");
		final Map<Class<?>, List<?>> m = new HashMap<>();
		final int count = 10;
		final OWLClass ann = OWLClassA.class.getAnnotation(OWLClass.class);
		final List<OWLClassA> aa = new ArrayList<>(count);
		m.put(OWLClassA.class, aa);
		for (int i = 0; i < count; i++) {
			final OWLClassA a = new OWLClassA();
			a.setUri(URI.create(BASE_A + i));
			a.setStringAttribute("stringAttribute" + i);
			final Set<String> s = new HashSet<String>();
			s.add(TYPE_A);
			s.add(ann.iri());
			a.setTypes(s);
			aa.add(a);
		}
		final List<OWLClassB> bb = new ArrayList<>(count);
		m.put(OWLClassB.class, bb);
		for (int i = 0; i < count; i++) {
			final OWLClassB b = new OWLClassB();
			b.setUri(URI.create(BASE_B + (i + 1)));
			b.setStringAttribute("strAtt" + (i + 1));
			bb.add(b);
		}
		final List<OWLClassC> cc = new ArrayList<>(count);
		m.put(OWLClassC.class, cc);
		for (int i = 0; i < count; i++) {
			final OWLClassC c = new OWLClassC();
			c.setUri(URI.create(BASE_C + i));
			if (i % 2 == 1) {
				c.setReferencedList(new ArrayList<>(aa));
			}
			cc.add(c);
		}
		final List<OWLClassD> dd = new ArrayList<>();
		m.put(OWLClassD.class, dd);
		for (int i = 0; i < count; i++) {
			final OWLClassD d = new OWLClassD();
			d.setUri(URI.create(BASE_D + i));
			d.setOwlClassA(aa.get(i));
			dd.add(d);
		}
		final List<OWLClassE> ee = new ArrayList<>();
		m.put(OWLClassE.class, ee);
		for (int i = 0; i < count; i++) {
			final OWLClassE e = new OWLClassE();
			// Auto-generated id
			e.setStringAttribute("eStr");
			ee.add(e);
		}
		return m;
	}

}

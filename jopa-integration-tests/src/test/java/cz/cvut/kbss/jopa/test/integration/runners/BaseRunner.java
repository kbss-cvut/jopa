package cz.cvut.kbss.jopa.test.integration.runners;

import cz.cvut.kbss.jopa.test.*;

import java.net.URI;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;

abstract class BaseRunner {

	protected static final URI CONTEXT_ONE = URI
			.create("http://krizik.felk.cvut.cz/jopa/contexts#One");
	protected static final URI CONTEXT_TWO = URI
			.create("http://krizik.felk.cvut.cz/jopa/contexts#Two");

	protected final Logger logger;

	protected OWLClassA entityA;
	protected OWLClassB entityB;
	protected OWLClassC entityC;
	protected OWLClassD entityD;
	// Generated IRI
	protected OWLClassE entityE;
	// Two relationships, cascade
	protected OWLClassG entityG;
	protected OWLClassH entityH;
	// Lazy reference to OWLClassA
	protected OWLClassI entityI;

	BaseRunner(Logger logger) {
		assert logger != null;
		this.logger = logger;
		init();
	}

	/**
	 * Initializes the test entities in the following manner:
	 * <ul>
	 * <li>entityA contains non-empty types</li>
	 * <li>entityB's properties are null</li>
	 * <li>entityC's simple and referenced lists are null</li>
	 * <li>entityD's reference to OWLClassA is set to entityA</li>
	 * <li>entityE's URI is left null for ID generation</li>
	 * <li>entityG's reference to OWLClassH is set to entityH</li>
	 * <li>entityH's reference to OWLClassA is set to entityA</li>
	 * <li>entityI's reference to OWLClassA is set to entityA</li>
	 * </ul>
	 */
	private void init() {
		entityA = new OWLClassA();
		entityA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA"));
		entityA.setStringAttribute("entityAStringAttribute");
		final Set<String> types = new HashSet<String>();
		types.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassU");
		entityA.setTypes(types);
		entityB = new OWLClassB();
		entityB.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityB"));
		entityB.setStringAttribute("entityBStringAttribute");
		entityC = new OWLClassC();
		entityC.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityC"));
		entityD = new OWLClassD();
		entityD.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityD"));
		entityD.setOwlClassA(entityA);
		entityE = new OWLClassE();
		entityE.setStringAttribute("entityEStringAttribute");
		entityH = new OWLClassH();
		entityH.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityH"));
		entityH.setOwlClassA(entityA);
		entityG = new OWLClassG();
		entityG.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityG"));
		entityG.setOwlClassH(entityH);
		entityI = new OWLClassI();
		entityI.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityI"));
		entityI.setOwlClassA(entityA);
	}
}

package cz.cvut.kbss.owlpersistence.owlapi;

import cz.cvut.kbss.owlpersistence.EntityManager;

public class OWLPersistence {

	private final static EntityManager em = new OWLAPIPersistenceConnector();

	public static EntityManager getEntityManager() {
		return em;
	}
}

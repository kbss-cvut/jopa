package cz.cvut.kbss.owlpersistence.owlapi;

import cz.cvut.kbss.owlpersistence.EntityManager;

public class OWLPersistence {

	private static EntityManager em = new OWLAPIPersistenceConnector();

	public static EntityManager getEntityManager() {
		return em;
	}
}

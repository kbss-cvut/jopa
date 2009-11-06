package cz.cvut.kbss.owlpersistence;

import java.lang.reflect.Field;
import java.util.logging.Level;
import java.util.logging.Logger;

import cz.cvut.kbss.owlpersistence.owlapi.OWLPersistence;

public aspect OWLPersistenceAspect {

	private static final Logger log = Logger
			.getLogger(OWLPersistenceAspect.class.getName());

	pointcut getter() : get( @(OWLObjectProperty || OWLDataProperty) * * ) && within(@OWLClass *);

	pointcut setter() : set( @(OWLObjectProperty || OWLDataProperty || RDFSLabel) * * ) && within(@OWLClass *);

	private final EntityManager m = OWLPersistence.getEntityManager();

	after() : setter()  {
		try {
			final Object object = thisJoinPoint.getTarget();
			final Field field = object.getClass().getDeclaredField(
					thisJoinPoint.getSignature().getName());

			field.setAccessible(true);

			if (m.contains(object)) {
				if (log.isLoggable(Level.CONFIG)) {
					log.config("*** Saving " + field.getName() + " of "
							+ object.getClass() + ":" + object.hashCode());
				}
				m.saveReference(object, field);
			}
		} catch (NoSuchFieldException e) {
			log.log(Level.SEVERE, e.getMessage(), e);
		}

	}

	before() : getter()  {
		try {
			final Object object = thisJoinPoint.getTarget();
			final Field field = object.getClass().getDeclaredField(
					thisJoinPoint.getSignature().getName());

			field.setAccessible(true);

			if (m.contains(object)) {
				if (log.isLoggable(Level.CONFIG)) {
					log.config("*** Fetching " + field.getName() + " of "
							+ object.getClass() + ":" + object.hashCode());
				}

				m.loadReference(object, field);
			}
		} catch (NoSuchFieldException e) {
			log.log(Level.SEVERE, e.getMessage(), e);
		}
	}
}

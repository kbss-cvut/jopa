package cz.cvut.kbss.owlpersistence.owlapi;

import java.lang.reflect.Field;

import cz.cvut.kbss.owlpersistence.model.OWLPersistenceException;
import cz.cvut.kbss.owlpersistence.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.owlpersistence.model.annotations.OWLClass;
import cz.cvut.kbss.owlpersistence.model.annotations.Types;
import cz.cvut.kbss.owlpersistence.model.annotations.OWLDataProperty;
import cz.cvut.kbss.owlpersistence.model.annotations.OWLObjectProperty;

public aspect BeanListenerAspect {

	// private static final Logger log =
	// Logger.getLogger(BeanListenerAspect.class
	// .getName());

	pointcut getter() : get( @(OWLObjectProperty || OWLDataProperty || Types) * * ) && within(@OWLClass *);

	pointcut setter() : set( @(OWLObjectProperty || OWLDataProperty || OWLAnnotationProperty || Types) * * ) && within(@OWLClass *);

	after() : setter()  {
		try {
			final Object object = thisJoinPoint.getTarget();
			final Field field = object.getClass().getDeclaredField(
					thisJoinPoint.getSignature().getName());

			field.setAccessible(true);
			//
			// if (log.isLoggable(Level.CONFIG)) {
			// log.config("*** Saving " + field.getName() + " of "
			// + object.getClass() + ":" + object.hashCode());
			// }
			OWLAPIPersistenceProvider.saveReference(object, field);
		} catch (NoSuchFieldException e) {
			// log.log(Level.SEVERE, e.getMessage(), e);
			throw new OWLPersistenceException();
		}

	}

	before() : getter()  {
		try {
			final Object object = thisJoinPoint.getTarget();
			final Field field = object.getClass().getDeclaredField(
					thisJoinPoint.getSignature().getName());

			field.setAccessible(true);

			// if (log.isLoggable(Level.CONFIG)) {
			// log.config("*** Fetching " + field.getName() + " of "
			// + object.getClass() + ":" + object.hashCode());
			// }

			OWLAPIPersistenceProvider.loadReference(object, field);
		} catch (NoSuchFieldException e) {
			// log.log(Level.SEVERE, e.getMessage(), e);
			throw new OWLPersistenceException();

		}
	}
}

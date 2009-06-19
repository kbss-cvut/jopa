package cz.cvut.kbss.owlpersistence.owlapi.aspectj;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import cz.cvut.kbss.owlpersistence.OWLObjectProperty;
import cz.cvut.kbss.owlpersistence.OWLSequence;
import cz.cvut.kbss.owlpersistence.FetchType;
import cz.cvut.kbss.owlpersistence.OWLClass;
import cz.cvut.kbss.owlpersistence.owlapi.OWLAPIPersistenceConnector;
import cz.cvut.kbss.owlpersistence.owlapi.OWLPersistence;

public aspect OWLPersistenceAspect {

	private static final Log log = LogFactory
			.getLog(OWLPersistenceAspect.class);

	pointcut getter() : get( @OWLObjectProperty(fetchType=FetchType.LAZY) * * ) && within(@OWLClass *);

	pointcut setter() : set( @OWLObjectProperty(fetchType=FetchType.LAZY) * * ) && within(@OWLClass *);

	before() : getter()  {
		lazyLoad(thisJoinPoint.getTarget(), thisJoinPoint.getSignature()
				.getName());
	}

	// after() : setter() {
	// save(thisJoinPoint.getTarget(), thisJoinPoint.getSignature()
	// .getName());
	// }
	//
	// private void saveLoad(final Object o, final String fieldName) {
	// if (log.isDebugEnabled()) {
	// log.debug("Lazily fetching " + fieldName + " of " + o);
	// }
	//
	// try {
	// final Field f = o.getClass().getDeclaredField(fieldName);
	// f.setAccessible(true);
	//
	// if (f.get(o) != null) {
	// return;
	// }
	//
	// if (f.getType().isAssignableFrom(List.class)) {
	// final OWLSequence seq = f.getAnnotation(OWLSequence.class);
	// ((OWLAPIPersistenceConnector) (OWLPersistence
	// .getEntityManager())).saveSequence(o, f, seq);
	//
	// } else if (f.getType().isAssignableFrom(Set.class)) {
	// final OWLObjectProperty p = f
	// .getAnnotation(OWLObjectProperty.class);
	// ((OWLAPIPersistenceConnector) (OWLPersistence
	// .getEntityManager())).saveSet(o, f, p);
	// } else {
	// final OWLObjectProperty p = f
	// .getAnnotation(OWLObjectProperty.class);
	// ((OWLAPIPersistenceConnector) (OWLPersistence
	// .getEntityManager())).saveObject(o, f, p);
	// }
	// } catch (IllegalAccessException e) {
	// log.error(e, e);
	// } catch (NoSuchFieldException e) {
	// log.error(e, e);
	// }
	// }

	private void lazyLoad(final Object o, final String fieldName) {
		if (log.isDebugEnabled()) {
			log.debug("Lazily fetching " + fieldName + " of " + o);
		}

		try {
			final Field f = o.getClass().getDeclaredField(fieldName);
			f.setAccessible(true);

			if (f.get(o) != null) {
				return;
			}

			if (f.getType().isAssignableFrom(List.class)) {
				((OWLAPIPersistenceConnector) (OWLPersistence
						.getEntityManager())).loadSequence(o, f, f
						.getAnnotation(OWLObjectProperty.class), f
						.getAnnotation(OWLSequence.class));

			} else if (f.getType().isAssignableFrom(Set.class)) {
				final OWLObjectProperty p = f
						.getAnnotation(OWLObjectProperty.class);
				((OWLAPIPersistenceConnector) (OWLPersistence
						.getEntityManager())).loadSet(o, f, p);
			} else {
				final OWLObjectProperty p = f
						.getAnnotation(OWLObjectProperty.class);
				((OWLAPIPersistenceConnector) (OWLPersistence
						.getEntityManager())).loadObject(o, f, p);
			}
		} catch (IllegalAccessException e) {
			log.error(e, e);
		} catch (NoSuchFieldException e) {
			log.error(e, e);
		}
	}
}

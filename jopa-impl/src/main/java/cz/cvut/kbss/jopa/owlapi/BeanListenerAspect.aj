/**
 * Copyright (C) 2011 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

package cz.cvut.kbss.jopa.owlapi;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.logging.Level;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.model.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.annotations.Properties;
import cz.cvut.kbss.jopa.model.annotations.Types;
import cz.cvut.kbss.jopa.sessions.CloneBuilderImpl;

public aspect BeanListenerAspect {

	private Object o;

	private static final Logger LOG = Logger.getLogger(BeanListenerAspect.class.getName());

	pointcut getter() : get( @(OWLObjectProperty || OWLDataProperty || Types || Properties ) * * ) && within(@OWLClass *);

	pointcut setter() : set( @(OWLObjectProperty || OWLDataProperty || Types || Properties ) * * ) && within(@OWLClass *);

	pointcut add(Collection c) : call(boolean Collection.add(..)) && target(c);

	pointcut remove(Collection c) : call(boolean Collection.remove(..)) && target(c);

	before() : setter() {
		final Object object = thisJoinPoint.getTarget();
		Class<?> cls = object.getClass();
		Field field = null;
		final String fieldName = thisJoinPoint.getSignature().getName();
		try {
			field = cls.getDeclaredField(fieldName);
		} catch (NoSuchFieldException e) {
			Class<?> superCls = cls;
			while ((superCls = superCls.getSuperclass()) != null) {
				try {
					field = superCls.getDeclaredField(fieldName);
				} catch (NoSuchFieldException ex) {
					// Do nothing, keep trying
				}
			}
			if (field == null) {
				throw new OWLPersistenceException(e.getMessage());
			}
		} catch (SecurityException e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
			throw new OWLPersistenceException(e.getMessage());
		}
		if (CloneBuilderImpl.isFieldInferred(field)) {
			throw new OWLPersistenceException("Modifying inferred attributes is forbidden.");
		}
	}

	after() returning : setter() {
		final Object entity = thisJoinPoint.getTarget();

		OWLAPIPersistenceProvider.persistEntityChanges(entity);
	}

	before() : getter()  {
		try {
			final Object object = thisJoinPoint.getTarget();
			o = object;
			final Field field = object.getClass().getDeclaredField(
					thisJoinPoint.getSignature().getName());

			field.setAccessible(true);

			if (LOG.isLoggable(Level.CONFIG)) {
				LOG.config("*** Fetching " + field.getName() + " of " + object.getClass() + ":"
						+ object.hashCode());
			}

			OWLAPIPersistenceProvider.loadReference(object, field);
		} catch (NoSuchFieldException e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
			throw new OWLPersistenceException();
		} catch (IllegalArgumentException e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
			throw new OWLPersistenceException();
		} catch (IllegalAccessException e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
			throw new OWLPersistenceException();
		}
	}

	after(Collection c) returning : add(c) {
		checkModification(c);
	}

	after(Collection c) returning : remove(c) {
		checkModification(c);
	}

	private void checkModification(Collection<?> c) {
		if (o == null) {
			return;
		}
		Field[] fields = o.getClass().getDeclaredFields();
		try {
			for (Field f : fields) {
				f.setAccessible(true);
				if (f.get(o) == c) {
					OWLAPIPersistenceProvider.persistEntityChanges(o);
					break;
				}
			}
		} catch (IllegalAccessException e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
			throw new OWLPersistenceException();
		}
	}
}

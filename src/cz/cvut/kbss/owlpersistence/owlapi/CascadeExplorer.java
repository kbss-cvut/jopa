package cz.cvut.kbss.owlpersistence.owlapi;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import cz.cvut.kbss.owlpersistence.CascadeType;
import cz.cvut.kbss.owlpersistence.OWLPersistenceException;
import cz.cvut.kbss.owlpersistence.owlapi.OWLAPIPersistenceConnector.OWLClassAnnotation;

abstract class CascadeExplorer {

	private OWLAPIPersistenceConnector pc;

	private CascadeType ct;

	public void start(final OWLAPIPersistenceConnector pc, final Object o,
			CascadeType ct) {
		this.ct = ct;
		this.pc = pc;

		_collectCascadedGraph(o, new HashSet<Object>(), new HashSet<Field>());
	}

	private void _collectCascadedGraph(final Object o,
			final Collection<Object> objects, final Set<Field> fields) {

		// Object is already marked
		if (objects.contains(o)) {
			return;
		}

		objects.add(o);

		explore(o);

		final OWLClassAnnotation a = pc.processOWLClass(o.getClass());

		try {
			for (final Field field : a.objectFields) {
				if (fields.contains(field)) {
					continue;
				}

				final List<CascadeType> ct = Arrays.asList(field.getAnnotation(
						cz.cvut.kbss.owlpersistence.OWLObjectProperty.class)
						.cascadeType());

				if (!ct.contains(CascadeType.ALL) && !ct.contains(ct)) {
					continue;
				}

				fields.add(field);

				final Collection col;

				if (field.getType().isAssignableFrom(List.class)
						|| field.getType().isAssignableFrom(Set.class)) {
					col = Collection.class.cast(field.get(o));
				} else {
					col = Collections.singleton(field.get(o));
				}

				if (col != null) {
					for (final Object o2 : col) {
						if (o2 == null) {
							continue;
						}
						_collectCascadedGraph(o2, objects, fields);
					}
				}
			}
		} catch (IllegalAccessException e) {
			throw new OWLPersistenceException(e);
		}
	}

	protected abstract void explore(final Object o);
}
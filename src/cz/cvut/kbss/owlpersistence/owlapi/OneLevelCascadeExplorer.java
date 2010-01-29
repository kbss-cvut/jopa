package cz.cvut.kbss.owlpersistence.owlapi;

import java.util.Arrays;
import java.util.List;

import cz.cvut.kbss.owlpersistence.model.OWLPersistenceException;
import cz.cvut.kbss.owlpersistence.model.annotations.CascadeType;
import cz.cvut.kbss.owlpersistence.model.metamodel.Attribute;
import cz.cvut.kbss.owlpersistence.model.metamodel.EntityType;

public abstract class OneLevelCascadeExplorer {

	private CascadeType ct;

	public void start(final AbstractEntityManager pc, final Object o, CascadeType ct) {
		this.ct = ct;

		final EntityType<?> a = pc.getMetamodel().entity(o.getClass());
		for (final Attribute<?, ?> at : a.getAttributes()) {

			final List<CascadeType> cTypes = Arrays
					.asList(at.getCascadeTypes());

			try {
				if (!cTypes.contains(CascadeType.ALL)
						&& !cTypes.contains(this.ct)) {
					exploreNonCascaded(at, o);
				} else {
					exploreCascaded(at, o);
				}
			} catch (Exception e) {
				throw new OWLPersistenceException(e);
			}
		}
	}

	protected void exploreCascaded(final Attribute<?, ?> at,
			final Object o) throws IllegalAccessException {
		// empty body
	}

	protected void exploreNonCascaded(final Attribute<?, ?> at,
			final Object o) throws IllegalAccessException {
		// empty body		
	}
}
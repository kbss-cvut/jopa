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
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import cz.cvut.kbss.jopa.accessors.OWLOntologyAccessor;
import cz.cvut.kbss.jopa.model.EntityManagerFactory;
import cz.cvut.kbss.jopa.model.LoadState;
import cz.cvut.kbss.jopa.model.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.PersistenceProvider;
import cz.cvut.kbss.jopa.model.ProviderUtil;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;

public class OWLAPIPersistenceProvider implements PersistenceProvider,
		ProviderUtil {

	private static Set<EntityManagerFactoryImpl> emfs = new HashSet<EntityManagerFactoryImpl>();

	public OWLAPIPersistenceProvider() {
	}

	public EntityManagerFactory createEntityManagerFactory(String emName,
			Map<String, String> map) {
		final EntityManagerFactoryImpl emf = new EntityManagerFactoryImpl(map);
		emfs.add(emf);
		return emf;
	}

	public ProviderUtil getProviderUtil() {
		return this;
	}

	public LoadState isLoaded(Object entity) {
		return LoadState.UNKNOWN;
	}

	public LoadState isLoadedWithReference(Object entity, String attributeName) {
		return LoadState.UNKNOWN;
	}

	public LoadState isLoadedWithoutReference(Object entity,
			String attributeName) {
		return LoadState.UNKNOWN;
	}

	private static UnitOfWorkImpl getPersistenceContext(Object entity) {
		if (entity == null) {
			return null;
		}
		for (EntityManagerFactoryImpl emf : emfs) {
			final UnitOfWorkImpl uow = emf.getServerSession()
					.getPersistenceContext(entity);
			if (uow != null) {
				return uow;
			}
		}
		return null;
	}

	static void loadReference(Object o, Field f)
			throws IllegalArgumentException, IllegalAccessException {
		final UnitOfWorkImpl uow = getPersistenceContext(o);

		if (uow != null) {
			Object managedOrig = uow.getOriginal(o);
			if (managedOrig == null) {
				throw new OWLPersistenceException(
						"Entity not managed in the current persistence context.");
			}
			Object val = f.get(managedOrig);
			if (val != null) {
				return;
			}
			uow.getOntologyAccessor().loadReference(o, f, uow);
		}
	}

	static void saveReference(Object o, Field f) {
		final UnitOfWorkImpl uow = getPersistenceContext(o);

		if (uow != null && uow.isInTransaction()) {
			OWLOntologyAccessor accessor = (OWLOntologyAccessor) uow
					.getOntologyAccessor();
			accessor.saveReference(o, f, uow);
		}
	}

	/**
	 * Write changes to the specified entity to the transaction ontology.
	 * 
	 * @param entity
	 *            Entity to persist
	 */
	static void persistEntityChanges(Object entity) {
		final UnitOfWorkImpl uow = getPersistenceContext(entity);
		if (uow != null && uow.isInTransaction()) {
			uow.persistChangeInTransaction(entity);
		}
	}
}

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

package cz.cvut.kbss.owlpersistence.owlapi;

import java.lang.reflect.Field;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import cz.cvut.kbss.owlpersistence.model.EntityManagerFactory;
import cz.cvut.kbss.owlpersistence.model.LoadState;
import cz.cvut.kbss.owlpersistence.model.PersistenceProvider;
import cz.cvut.kbss.owlpersistence.model.ProviderUtil;

public class OWLAPIPersistenceProvider implements PersistenceProvider,
		ProviderUtil {

	private static Set<EntityManagerFactoryImpl> emfs = new HashSet<EntityManagerFactoryImpl>();

	public OWLAPIPersistenceProvider() {
	}

	
	public EntityManagerFactory createEntityManagerFactory(String emName,
			Map map) {
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

	private static AbstractEntityManager find(Object o) {
		for (final EntityManagerFactoryImpl emfi : emfs) {
			for (final AbstractEntityManager emi : emfi.getEntityManagers()) {
				if (emi.isOpen() && emi.contains(o)) {
					return emi;
				}
			}
		}

		return null;
	}

	static void loadReference(Object o, Field f) {
		final AbstractEntityManager ei = find(o);

		if (ei != null) {
			ei.loadReference(o, f);
		}
	}

	static void saveReference(Object o, Field f) {
		final AbstractEntityManager ei = find(o);

		if (ei != null) {
			ei.saveReference(o, f);
		}
	}
}

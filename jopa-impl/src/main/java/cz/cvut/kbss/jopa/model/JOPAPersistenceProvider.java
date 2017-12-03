/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.exceptions.OWLInferredAttributeModifiedException;
import cz.cvut.kbss.jopa.sessions.CloneBuilderImpl;

import java.lang.reflect.Field;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

public class JOPAPersistenceProvider implements PersistenceProvider, ProviderUtil {

    private static Set<EntityManagerFactoryImpl> emfs = new HashSet<>();

    @Override
    public EntityManagerFactoryImpl createEntityManagerFactory(String emName, Map<String, String> properties) {
        final EntityManagerFactoryImpl emf = new EntityManagerFactoryImpl(properties);
        emfs.add(emf);
        return emf;
    }

    @Override
    public ProviderUtil getProviderUtil() {
        return this;
    }

    @Override
    public LoadState isLoaded(Object entity) {
        final Optional<EntityManagerFactoryImpl> found = emfs.stream().filter(emf -> emf.isLoaded(entity)).findAny();
        return found.map(entityManagerFactory -> LoadState.LOADED).orElse(LoadState.UNKNOWN);
    }

    @Override
    public LoadState isLoadedWithReference(Object entity, String attributeName) {
        return isLoadedWithoutReference(entity, attributeName);
    }

    @Override
    public LoadState isLoadedWithoutReference(Object entity, String attributeName) {
        final Optional<EntityManagerFactoryImpl> found = emfs.stream()
                                                             .filter(emf -> emf.isLoaded(entity, attributeName))
                                                             .findAny();
        return found.map(entityManagerFactory -> LoadState.LOADED).orElse(LoadState.UNKNOWN);
    }

    static void verifyInferredAttributeNotModified(Object entity, Field field) {
        if (CloneBuilderImpl.isFieldInferred(field)) {
            throw new OWLInferredAttributeModifiedException(
                    String.format("Field %s of instance %s is inferred and cannot be modified.", field, entity));
        }
    }
}

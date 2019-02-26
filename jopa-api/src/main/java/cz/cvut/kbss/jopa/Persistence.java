/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa;

import cz.cvut.kbss.jopa.model.*;

import java.util.*;

/**
 * Bootstrap class that is used to obtain an {@link EntityManagerFactory}.
 * <p>
 * The {@code Persistence} class is also used to obtain a {@link PersistenceUtil} instance.
 */
public class Persistence {

    private static final PersistenceUtil pu = new PersistenceUtilImpl();

    private Persistence() {
        throw new AssertionError("No Persistence instances can be created.");
    }

    /**
     * Create and return an EntityManagerFactory for the named persistence unit.
     *
     * @param persistenceUnitName the name of the persistence unit
     * @return the factory that creates EntityManagers configured according to the specified persistence unit
     */
    public static EntityManagerFactory createEntityManagerFactory(final String persistenceUnitName) {
        return createEntityManagerFactory(persistenceUnitName, Collections.emptyMap());
    }

    /**
     * Create and return an EntityManagerFactory for the named persistence unit using the given properties.
     *
     * @param persistenceUnitName the name of the persistence unit
     * @param properties          Additional properties to use when creating the factory. The values of these properties
     *                            override any values that may have been configured elsewhere
     * @return the factory that creates EntityManagers configured according to the specified persistence unit
     */
    public static EntityManagerFactory createEntityManagerFactory(final String persistenceUnitName,
                                                                  final Map<String, String> properties) {
        final Map<String, String> realParams = new HashMap<>(properties);

        final String className = realParams.get(PersistenceProperties.JPA_PERSISTENCE_PROVIDER);

        if (className == null) {
            throw new IllegalArgumentException("Missing persistence unit provider.");
        }
        final List<PersistenceProvider> providers = PersistenceProviderResolverHolder
                .getPersistenceProviderResolver().getPersistenceProviders();

        final Optional<PersistenceProvider> provider = providers.stream().filter(pp -> pp.getClass().getName()
                                                                                         .equals(className))
                                                                .findFirst();

        return provider.orElseThrow(() -> new IllegalArgumentException(
                "Type " + className + " is not a PersistenceProvider implementation."))
                       .createEntityManagerFactory(persistenceUnitName, realParams);
    }

    /**
     * Return the PersistenceUtil instance
     *
     * @return PersistenceUtil instance
     */
    public static PersistenceUtil getPersistenceUtil() {
        return pu;
    }
}

class PersistenceUtilImpl implements PersistenceUtil {

    @Override
    public boolean isLoaded(Object entity, String attributeName) {
        for (final PersistenceProvider pp : PersistenceProviderResolverHolder.getPersistenceProviderResolver()
                                                                             .getPersistenceProviders()) {

            switch (pp.getProviderUtil().isLoadedWithoutReference(entity, attributeName)) {
                case LOADED:
                    return true;
                case NOT_LOADED:
                    return false;
                default:
                    break;
            }
        }

        for (final PersistenceProvider pp : PersistenceProviderResolverHolder.getPersistenceProviderResolver()
                                                                             .getPersistenceProviders()) {

            switch (pp.getProviderUtil().isLoadedWithReference(entity, attributeName)) {
                case LOADED:
                    return true;
                case NOT_LOADED:
                    return false;
                default:
                    break;
            }
        }
        return false;
    }

    @Override
    public boolean isLoaded(Object entity) {
        for (final PersistenceProvider pp : PersistenceProviderResolverHolder.getPersistenceProviderResolver()
                                                                             .getPersistenceProviders()) {

            switch (pp.getProviderUtil().isLoaded(entity)) {
                case LOADED:
                    return true;
                case NOT_LOADED:
                    return false;
                default:

            }
        }

        return false;
    }
}

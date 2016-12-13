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
package cz.cvut.kbss.jopa;

import cz.cvut.kbss.jopa.model.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

public class Persistence {

    private static final Logger LOG = LoggerFactory.getLogger(Persistence.class);

    private static final Set<PersistenceProvider> map = new HashSet<>();

    private static final PersistenceUtil pu = new PersistenceUtilImpl();

    static {
        // TODO load persistence providers
    }

    private Persistence() {
        throw new AssertionError();
    }

    /**
     * Create and return an EntityManagerFactory for the named persistence unit.
     *
     * @param persistenceUnitName the name of the persistence unit
     * @return the factory that creates EntityManagers configured according to the specified persistence unit.
     */
    public static EntityManagerFactory createEntityManagerFactory(final String persistenceUnitName) {
        return createEntityManagerFactory(persistenceUnitName, Collections.<String, String>emptyMap());
    }

    public static EntityManagerFactory createEntityManagerFactory(final String persistenceUnitName,
                                                                  final Map<String, String> parameters) {
        final Map<String, String> realParams = new HashMap<>();

        realParams.putAll(parameters);

        final String className = realParams.get(PersistenceProperties.JPA_PERSISTENCE_PROVIDER);

        if (className == null) {
            throw new IllegalArgumentException("Persistent unit provider unknown.");
        }

        try {
            final PersistenceProvider pp = ((Class<PersistenceProvider>) Class.forName(className)).newInstance();

            // TODO get at runtime
            map.add(pp);

            return pp.createEntityManagerFactory(persistenceUnitName, realParams);
        } catch (Exception e) {
            LOG.error(e.getMessage(), e);
            throw new IllegalArgumentException("Problems with creating EntityManagerFactory.", e);
        }
    }

    /**
     * @return PersistenceUtil instance
     */
    public static PersistenceUtil getPersistenceUtil() {
        return pu;
    }

    private static PersistenceProvider getProvider(final Object o) {
        for (final PersistenceProvider emp : map) {
            if (!emp.getProviderUtil().isLoaded(o).equals(LoadState.UNKNOWN)) {
                return emp;
            }
        }
        return null;
    }
}

class PersistenceUtilImpl implements PersistenceUtil {

    @Override
    public boolean isLoaded(Object entity, String attributeName) {
        for (final PersistenceProvider pp : PersistenceProviderResolverHolder
                .getPersistenceProviderResolver().getPersistenceProviders()) {

            switch (pp.getProviderUtil().isLoadedWithoutReference(entity, attributeName)) {
                case LOADED:
                    return true;
                case NOT_LOADED:
                    return false;
                default:
                    break;
            }
        }

        for (final PersistenceProvider pp : PersistenceProviderResolverHolder
                .getPersistenceProviderResolver().getPersistenceProviders()) {

            switch (pp.getProviderUtil().isLoadedWithReference(entity, attributeName)) {
                case LOADED:
                    return true;
                case NOT_LOADED:
                    return false;
                default:
                    break;
            }
        }
        return true;
    }

    @Override
    public boolean isLoaded(Object entity) {
        for (final PersistenceProvider pp : PersistenceProviderResolverHolder
                .getPersistenceProviderResolver().getPersistenceProviders()) {

            switch (pp.getProviderUtil().isLoaded(entity)) {
                case LOADED:
                    return true;
                case NOT_LOADED:
                    return false;
                default:

            }
        }

        return true;
    }
}

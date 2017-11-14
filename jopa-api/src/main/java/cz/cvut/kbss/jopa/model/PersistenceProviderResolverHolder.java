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

/**
 * Holds the global {@link PersistenceProviderResolver} instance.
 * <p>
 * If no {@code PersistenceProviderResolver} is set by the environment, the default {@code PersistenceProviderResolver}
 * is used. Implementations must be thread-safe.
 */
public class PersistenceProviderResolverHolder {

    private static PersistenceProviderResolver instance = new DefaultPersistenceProviderResolver();

    private PersistenceProviderResolverHolder() {
        throw new AssertionError();
    }

    /**
     * Returns the current persistence provider resolver.
     *
     * @return the current persistence provider resolver
     */
    public static synchronized PersistenceProviderResolver getPersistenceProviderResolver() {
        if (instance == null) {
            instance = new DefaultPersistenceProviderResolver();
        }
        return instance;
    }

    /**
     * Defines the persistence provider resolver used.
     *
     * @param resolver persistence provider resolver to be used
     */
    public static synchronized void setPersistenceProviderResolver(final PersistenceProviderResolver resolver) {
        instance = resolver;
    }
}

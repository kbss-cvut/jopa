/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model;

import java.util.ArrayList;
import java.util.List;

public class PersistenceProviderResolverHolder {

    private static PersistenceProviderResolver instance = null;

    private PersistenceProviderResolverHolder() {
        throw new AssertionError();
    }

    public static synchronized PersistenceProviderResolver getPersistenceProviderResolver() {
        if (instance == null) {
            instance = new PersistenceProviderResolver() {

                // TODO
                private List<PersistenceProvider> pp = new ArrayList<>();

                @Override
                public void clearCachedProviders() {
                    pp.clear();
                }

                @Override
                public List<PersistenceProvider> getPersistenceProviders() {
                    return pp;
                }
            };
        }

        return instance;
    }

    public static synchronized void setPersistenceProviderResolver(
            final PersistenceProviderResolver ppr) {
        instance = ppr;
    }
}

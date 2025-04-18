/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.model;

import java.util.Map;

/**
 * Interface implemented by the persistence provider.
 * <p>
 * It is invoked by the Persistence class in Java SE environments to create an EntityManagerFactory and/or to cause
 * schema generation to occur.
 */
public interface PersistenceProvider {

    /**
     * Called by Persistence class when an EntityManagerFactory is to be created.
     *
     * @param emName the name of the persistence unit
     * @param map    a Map of properties for use by the persistence provider. These properties specify storage
     *               connection configuration and may also specify additional configuration.
     * @return EntityManagerFactory for the persistence unit, or null if the provider is not the right provider.
     */
    EntityManagerFactory createEntityManagerFactory(String emName, Map<String, String> map);

    /**
     * Return the utility interface implemented by the persistence provider.
     *
     * @return ProviderUtil interface
     */
    ProviderUtil getProviderUtil();
}

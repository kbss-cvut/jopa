/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;

public class JOPAPersistenceProvider implements PersistenceProvider, ProviderUtil {

    private static final Logger LOG = LoggerFactory.getLogger(JOPAPersistenceProvider.class);

    private static final Set<EntityManagerFactoryImpl> EMFS = Collections.synchronizedSet(new HashSet<>());

    public JOPAPersistenceProvider() {
        logVersionInfo();
    }

    @Override
    public EntityManagerFactoryImpl createEntityManagerFactory(String emName, Map<String, String> properties) {
        final EntityManagerFactoryImpl emf = new EntityManagerFactoryImpl(properties, this::emfClosed);
        EMFS.add(emf);
        return emf;
    }

    private static void logVersionInfo() {
        try {
            final Properties props = new Properties();
            props.load(JOPAPersistenceProvider.class.getClassLoader().getResourceAsStream("jopa.properties"));
            assert props.containsKey("cz.cvut.jopa.version");
            assert props.containsKey("cz.cvut.jopa.build.timestamp");
            LOG.info("This is JOPA {}, built on {}...", props.get("cz.cvut.jopa.version"), props.get("cz.cvut.jopa.build.timestamp"));
        } catch (IOException e) {
            LOG.warn("Unable to load properties file to log version info.", e);
        }
    }

    void emfClosed(EntityManagerFactoryImpl emf) {
        EMFS.remove(emf);
    }

    @Override
    public ProviderUtil getProviderUtil() {
        return this;
    }

    @Override
    public LoadState isLoaded(Object entity) {
        final Optional<EntityManagerFactoryImpl> found = EMFS.stream().filter(emf -> emf.isLoaded(entity)).findAny();
        return found.map(entityManagerFactory -> LoadState.LOADED).orElse(LoadState.UNKNOWN);
    }

    @Override
    public LoadState isLoadedWithReference(Object entity, String attributeName) {
        return isLoadedWithoutReference(entity, attributeName);
    }

    @Override
    public LoadState isLoadedWithoutReference(Object entity, String attributeName) {
        final Optional<EntityManagerFactoryImpl> found = EMFS.stream()
                                                             .filter(emf -> emf.isLoaded(entity, attributeName))
                                                             .findAny();
        return found.map(entityManagerFactory -> LoadState.LOADED).orElse(LoadState.UNKNOWN);
    }
}

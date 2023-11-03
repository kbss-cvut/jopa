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

import cz.cvut.kbss.jopa.environment.TestPersistenceProvider;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;


class DefaultPersistenceProviderResolverTest {

    private final DefaultPersistenceProviderResolver resolver = new DefaultPersistenceProviderResolver();

    @AfterEach
    void tearDown() throws Exception {
        generateProviderFileContent(TestPersistenceProvider.class.getName());    // Clear the file content
    }

    @Test
    void getPersistenceProviderCachesProviderInstances() {
        final List<PersistenceProvider> providersOne = resolver.getPersistenceProviders();
        final List<PersistenceProvider> providersTwo = resolver.getPersistenceProviders();
        assertEquals(providersOne.size(), providersTwo.size());
        for (int i = 0; i < providersOne.size(); i++) {
            assertSame(providersOne.get(i), providersTwo.get(i));
        }
    }

    @Test
    void clearCachedProvidersEvictsProviderCache() {
        final List<PersistenceProvider> providersOne = resolver.getPersistenceProviders();
        resolver.clearCachedProviders();
        final List<PersistenceProvider> providersTwo = resolver.getPersistenceProviders();
        assertEquals(providersOne.size(), providersTwo.size());
        for (int i = 0; i < providersOne.size(); i++) {
            assertNotSame(providersOne.get(i), providersTwo.get(i));
        }
    }

    @Test
    void getProvidersReturnsProvidersFoundOnClasspathViaMetaInfConfiguration() {
        final List<PersistenceProvider> result = resolver.getPersistenceProviders();
        assertFalse(result.isEmpty());
        final Optional<PersistenceProvider> pp = result.stream().filter(p -> p instanceof TestPersistenceProvider)
                                                       .findAny();
        assertTrue(pp.isPresent());
    }

    @Test
    void registerProviderClassSkipsClassesConfiguredOnClasspathButNotFound() throws Exception {
        generateProviderFileContent("cz.cvut.kbss.jopa.UnknownClass");
        final List<PersistenceProvider> result = resolver.getPersistenceProviders();
        assertNotNull(result);
    }

    private void generateProviderFileContent(String content) throws Exception {
        final File file = new File(Thread.currentThread().getContextClassLoader().getResource(
                "META-INF/services/" + PersistenceProperties.JPA_PERSISTENCE_PROVIDER).getFile());
        Files.write(file.toPath(), content.getBytes(), StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE);
    }
}

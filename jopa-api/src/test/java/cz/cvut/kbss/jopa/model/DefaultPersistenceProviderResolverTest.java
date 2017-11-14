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

import cz.cvut.kbss.jopa.environment.TestPersistenceProvider;
import org.junit.After;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.io.File;
import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import static org.junit.Assert.*;

public class DefaultPersistenceProviderResolverTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    private DefaultPersistenceProviderResolver resolver = new DefaultPersistenceProviderResolver();

    @After
    public void tearDown() throws Exception {
        final Field typesField = DefaultPersistenceProviderResolver.class.getDeclaredField("PROVIDER_TYPES");
        typesField.setAccessible(true);
        ((Set) typesField.get(null)).clear();
        generateProviderFileContent("");    // Clear the file content
    }

    @Test
    public void getPersistenceProvidersInstantiatesProvidersOfRegisteredTypes() {
        DefaultPersistenceProviderResolver.registerPersistenceProviderClass(TestPersistenceProvider.class);
        final List<PersistenceProvider> result = resolver.getPersistenceProviders();
        assertFalse(result.isEmpty());
        final Optional<PersistenceProvider> pp = result.stream().filter(p -> p instanceof TestPersistenceProvider)
                                                       .findAny();
        assertTrue(pp.isPresent());
    }

    @Test
    public void getPersistenceProviderCachesProviderInstances() {
        DefaultPersistenceProviderResolver.registerPersistenceProviderClass(TestPersistenceProvider.class);
        final List<PersistenceProvider> providersOne = resolver.getPersistenceProviders();
        final List<PersistenceProvider> providersTwo = resolver.getPersistenceProviders();
        assertEquals(providersOne.size(), providersTwo.size());
        for (int i = 0; i < providersOne.size(); i++) {
            assertSame(providersOne.get(i), providersTwo.get(i));
        }
    }

    @Test
    public void clearCachedProvidersEvictsProviderCache() {
        DefaultPersistenceProviderResolver.registerPersistenceProviderClass(TestPersistenceProvider.class);
        final List<PersistenceProvider> providersOne = resolver.getPersistenceProviders();
        resolver.clearCachedProviders();
        final List<PersistenceProvider> providersTwo = resolver.getPersistenceProviders();
        assertEquals(providersOne.size(), providersTwo.size());
        for (int i = 0; i < providersOne.size(); i++) {
            assertNotSame(providersOne.get(i), providersTwo.get(i));
        }
    }

    @Test
    public void getProvidersSkipsNonInstantiableProviderClasses() {
        DefaultPersistenceProviderResolver.registerPersistenceProviderClass(InvalidPersistenceProvider.class);
        final List<PersistenceProvider> result = resolver.getPersistenceProviders();
        assertNotNull(result);
        final Optional<PersistenceProvider> pp = result.stream().filter(p -> p instanceof InvalidPersistenceProvider)
                                                       .findAny();
        assertFalse(pp.isPresent());
    }

    @Test
    public void getProvidersReturnsProvidersFoundOnClasspathViaMetaInfConfiguration() throws Exception {
        generateProviderFileContent("cz.cvut.kbss.jopa.environment.TestPersistenceProvider");
        final List<PersistenceProvider> result = resolver.getPersistenceProviders();
        assertFalse(result.isEmpty());
        final Optional<PersistenceProvider> pp = result.stream().filter(p -> p instanceof TestPersistenceProvider)
                                                       .findAny();
        assertTrue(pp.isPresent());
    }

    @Test
    public void registerProviderClassSkipsClassesConfiguredOnClasspathButNotFound() throws Exception {
        generateProviderFileContent("cz.cvut.kbss.jopa.UnknownClass");
        final List<PersistenceProvider> result = resolver.getPersistenceProviders();
        assertNotNull(result);
    }

    private void generateProviderFileContent(String content) throws Exception {
        final File file = new File(Thread.currentThread().getContextClassLoader()
                                         .getResource(DefaultPersistenceProviderResolver.PROVIDER_FILE).getFile());
        Files.write(file.toPath(), content.getBytes(), StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE);
    }

    @Test
    public void registerProviderClassSkipsClassConfiguredOnClasspathAndNotPersistenceProviderImplementation()
            throws Exception {
        generateProviderFileContent(DefaultPersistenceProviderResolver.class.getName());
        final List<PersistenceProvider> result = resolver.getPersistenceProviders();
        assertNotNull(result);
        result.forEach(r -> assertFalse(r instanceof DefaultPersistenceProviderResolver));
    }

    public static class InvalidPersistenceProvider implements PersistenceProvider {

        public InvalidPersistenceProvider(String puName) {
            // This will not work
        }

        @Override
        public EntityManagerFactory createEntityManagerFactory(String emName, Map<String, String> map) {
            return null;
        }

        @Override
        public ProviderUtil getProviderUtil() {
            return null;
        }
    }
}
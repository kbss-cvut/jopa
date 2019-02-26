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
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.util.Collections;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.mock;

public class PersistenceTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void createEmfWithPropertiesInstantiatesPersistenceProviderFromProperties() {
        DefaultPersistenceProviderResolver.registerPersistenceProviderClass(TestPersistenceProvider.class);
        final Map<String, String> props = Collections.singletonMap(PersistenceProperties.JPA_PERSISTENCE_PROVIDER,
                TestPersistenceProvider.class.getName());
        Persistence.createEntityManagerFactory("testPU", props);
        assertNotNull(TestPersistenceProvider.instance);
        assertEquals(1, TestPersistenceProvider.instance.createEmfCalled);
    }

    @Test
    public void createEmfThrowsIllegalArgumentWhenProviderIsNotConfigured() {
        DefaultPersistenceProviderResolver.registerPersistenceProviderClass(TestPersistenceProvider.class);
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage("Missing persistence unit provider.");
        Persistence.createEntityManagerFactory("testPU", Collections.emptyMap());
    }

    @Test
    public void createEmfThrowsIllegalArgumentWhenConfiguredClassIsNotPersistenceProvider() {
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage(
                "Type " + PersistenceTest.class.getName() + " is not a PersistenceProvider implementation.");
        final Map<String, String> props = Collections.singletonMap(PersistenceProperties.JPA_PERSISTENCE_PROVIDER,
                PersistenceTest.class.getName());
        Persistence.createEntityManagerFactory("testPU", props);
    }

    public static class TestPersistenceProvider implements PersistenceProvider {

        private static TestPersistenceProvider instance;

        private EntityManagerFactory emfMock = mock(EntityManagerFactory.class);
        private ProviderUtil providerUtilMock = mock(ProviderUtil.class);

        private int createEmfCalled = 0;
        private int getProviderUtilCalled = 0;

        public TestPersistenceProvider() {
            instance = this;
        }

        @Override
        public EntityManagerFactory createEntityManagerFactory(String emName, Map<String, String> map) {
            createEmfCalled++;
            return emfMock;
        }

        @Override
        public ProviderUtil getProviderUtil() {
            getProviderUtilCalled++;
            return providerUtilMock;
        }
    }
}
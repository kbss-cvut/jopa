/**
 * Copyright (C) 2023 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.environment.TestPersistenceProvider;
import cz.cvut.kbss.jopa.model.PersistenceProperties;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.Map;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class PersistenceTest {

    @Test
    void createEmfWithPropertiesInstantiatesPersistenceProviderFromProperties() {
        final Map<String, String> props = Collections.singletonMap(PersistenceProperties.JPA_PERSISTENCE_PROVIDER,
                TestPersistenceProvider.class.getName());
        Persistence.createEntityManagerFactory("testPU", props);
        Assertions.assertNotNull(TestPersistenceProvider.getInstance());
        Assertions.assertEquals(1, TestPersistenceProvider.getInstance().getCreateEmfCalled());
    }

    @Test
    void createEmfThrowsIllegalArgumentWhenProviderIsNotConfigured() {
        final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Persistence.createEntityManagerFactory("testPU", Collections.emptyMap()));
        Assertions.assertEquals("Missing persistence unit provider.", ex.getMessage());
    }

    @Test
    void createEmfThrowsIllegalArgumentWhenConfiguredClassIsNotPersistenceProvider() {
        final Map<String, String> props = Collections.singletonMap(PersistenceProperties.JPA_PERSISTENCE_PROVIDER,
                PersistenceTest.class.getName());
        final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Persistence.createEntityManagerFactory("testPU", props));
        assertThat(ex.getMessage(), containsString(
                "Type " + PersistenceTest.class.getName() + " is not a PersistenceProvider implementation"));
    }
}

/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.plugin;

import cz.cvut.kbss.jopa.environment.utils.TestPlugin;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.utils.Configuration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PersistenceUnitPluginExecutorTest {

    @BeforeEach
    void setUp() {
        TestPlugin.afterCreatedCalled = false;
        TestPlugin.beforeDestroyedCalled = false;
    }

    @Test
    void afterPersistenceUnitCreatedInvokesAfterPersistenceUnitCreatedMethodOnConfiguredPlugins() {
        final Configuration configuration = new Configuration(Map.of(JOPAPersistenceProperties.PERSISTENCE_UNIT_LIFECYCLE_PLUGINS, TestPlugin.class.getName()));
        final PersistenceUnitPluginExecutor sut = new PersistenceUnitPluginExecutor(configuration);

        sut.afterPersistenceUnitCreated(() -> null);
        assertTrue(TestPlugin.afterCreatedCalled);
        assertFalse(TestPlugin.beforeDestroyedCalled);
    }

    @Test
    void beforePersistenceUnitDestroyedInvokesBeforePersistenceUnitDestroyedMethodOnConfiguredPlugins() {
        final Configuration configuration = new Configuration(Map.of(JOPAPersistenceProperties.PERSISTENCE_UNIT_LIFECYCLE_PLUGINS, TestPlugin.class.getName()));
        final PersistenceUnitPluginExecutor sut = new PersistenceUnitPluginExecutor(configuration);

        sut.beforePersistenceUnitDestroyed(() -> null);
        assertTrue(TestPlugin.beforeDestroyedCalled);
        assertFalse(TestPlugin.afterCreatedCalled);
    }

    @Test
    void initializationHandlesEmptyConfiguration() {
        final Configuration configuration = new Configuration(Map.of());
        final PersistenceUnitPluginExecutor sut = new PersistenceUnitPluginExecutor(configuration);

        sut.afterPersistenceUnitCreated(() -> null);
        sut.beforePersistenceUnitDestroyed(() -> null);
        assertFalse(TestPlugin.afterCreatedCalled);
        assertFalse(TestPlugin.beforeDestroyedCalled);
    }
}

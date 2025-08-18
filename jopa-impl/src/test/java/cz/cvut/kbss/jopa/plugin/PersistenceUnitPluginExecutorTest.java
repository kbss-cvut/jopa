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

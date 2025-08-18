package cz.cvut.kbss.jopa.environment.utils;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.plugin.PersistenceUnitLifecyclePlugin;

public class TestPlugin implements PersistenceUnitLifecyclePlugin {

    public static boolean beforeDestroyedCalled = false;
    public static boolean afterCreatedCalled = false;

    @Override
    public void afterPersistenceUnitCreated(EntityManager em) {
        TestPlugin.afterCreatedCalled = true;
    }

    @Override
    public void beforePersistenceUnitDestroyed(EntityManager em) {
        TestPlugin.beforeDestroyedCalled = true;
    }
}

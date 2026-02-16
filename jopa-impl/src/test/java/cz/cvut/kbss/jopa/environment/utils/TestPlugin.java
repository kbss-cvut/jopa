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

/**
 * Copyright (C) 2020 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.model.EntityManagerFactory;
import cz.cvut.kbss.jopa.model.PersistenceProvider;
import cz.cvut.kbss.jopa.model.ProviderUtil;

import java.util.Map;

import static org.mockito.Mockito.mock;

public class TestPersistenceProvider implements PersistenceProvider {

    private static TestPersistenceProvider instance;

    private final EntityManagerFactory emfMock = mock(EntityManagerFactory.class);
    private final ProviderUtil providerUtilMock = mock(ProviderUtil.class);

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

    public static TestPersistenceProvider getInstance() {
        return instance;
    }

    public int getCreateEmfCalled() {
        return createEmfCalled;
    }

    public int getGetProviderUtilCalled() {
        return getProviderUtilCalled;
    }
}

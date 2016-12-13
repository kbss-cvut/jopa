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

import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.jopa.sessions.ServerSessionStub;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.jopa.sessions.cache.DisabledCacheManager;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;
import java.util.Set;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.*;

public class JOPAPersistenceProviderTest {

    @Mock
    private EntityManagerFactoryImpl emfMock;

    @Mock
    private ConnectionWrapper connectorMock;

    private ServerSessionStub serverSessionMock;

    private UnitOfWorkImpl uow;

    @Mock
    private MetamodelImpl metamodelMock;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.serverSessionMock = spy(new ServerSessionStub(connectorMock));
        when(serverSessionMock.getMetamodel()).thenReturn(metamodelMock);
        when(serverSessionMock.getLiveObjectCache()).thenReturn(new DisabledCacheManager());
        this.uow = spy(new UnitOfWorkImpl(serverSessionMock));
        doReturn(uow).when(serverSessionMock).acquireUnitOfWork();
        when(emfMock.getMetamodel()).thenReturn(metamodelMock);
        when(emfMock.getServerSession()).thenReturn(serverSessionMock);
        MetamodelMocks mocks = new MetamodelMocks();
        mocks.setMocks(metamodelMock);
        initPersistenceProvider();
    }

    private void initPersistenceProvider() throws Exception {
        final Field emfsField = JOPAPersistenceProvider.class.getDeclaredField("emfs");
        emfsField.setAccessible(true);
        final Set<EntityManagerFactoryImpl> emfs = (Set<EntityManagerFactoryImpl>) emfsField.get(null);
        emfs.add(emfMock);
    }

    @After
    public void tearDown() throws Exception {
        final Field emfsField = JOPAPersistenceProvider.class.getDeclaredField("emfs");
        emfsField.setAccessible(true);
        final Set<EntityManagerFactoryImpl> emfs = (Set<EntityManagerFactoryImpl>) emfsField.get(null);
        emfs.clear();
    }

    @Test
    public void getEntityFieldReturnsFieldOfEntityManagedInExistingPersistenceContext() throws Exception {
        final OWLClassA a = Generators.generateOwlClassAInstance();
        uow.registerNewObject(a, new EntityDescriptor());
        when(serverSessionMock.getPersistenceContext(a)).thenReturn(uow);

        final Field f = JOPAPersistenceProvider.getEntityField(a, OWLClassA.getStrAttField().getName());
        assertEquals(OWLClassA.getStrAttField(), f);
    }

    @Test
    public void getEntityFieldReturnsFieldOfEntityDeclaredInMappedSuperclass() throws Exception {
        final OWLClassQ instance = new OWLClassQ();
        instance.setUri(Generators.createIndividualIdentifier());
        uow.registerNewObject(instance, new EntityDescriptor());
        when(serverSessionMock.getPersistenceContext(instance)).thenReturn(uow);

        final Field result =
                JOPAPersistenceProvider.getEntityField(instance, OWLClassQ.getParentStringField().getName());
        assertEquals(OWLClassQ.getParentStringField(), result);
    }

    @Test
    public void getEntityFieldReturnsFieldOfEntityDeclaredInSuperclassEntity() throws Exception {
        final OWLClassR instance = initEntityWithSuperclassEntity();

        final Field result = JOPAPersistenceProvider.getEntityField(instance, OWLClassS.getNameField().getName());
        assertEquals(OWLClassS.getNameField(), result);
    }

    private OWLClassR initEntityWithSuperclassEntity() {
        final OWLClassR instance = new OWLClassR();
        instance.setUri(Generators.createIndividualIdentifier());
        uow.registerNewObject(instance, new EntityDescriptor());
        when(serverSessionMock.getPersistenceContext(instance)).thenReturn(uow);
        return instance;
    }

    @Test
    public void getEntityFieldReturnsFieldOfEntityWithEntitySupertype() throws Exception {
        final OWLClassR instance = initEntityWithSuperclassEntity();

        final Field result = JOPAPersistenceProvider.getEntityField(instance, OWLClassR.getOwlClassAField().getName());
        assertEquals(OWLClassR.getOwlClassAField(), result);
    }

    @Test
    public void getEntityFieldReturnsNullForNonManagedEntity() throws Exception {
        final OWLClassA a = Generators.generateOwlClassAInstance();

        assertNull(JOPAPersistenceProvider.getEntityField(a, OWLClassA.getStrAttField().getName()));
    }

    @Test
    public void getEntityFieldReturnsNullForNonPersistentField() throws Exception {
        final OWLClassO instance = new OWLClassO();
        instance.setUri(Generators.createIndividualIdentifier());
        uow.registerNewObject(instance, new EntityDescriptor());
        when(serverSessionMock.getPersistenceContext(instance)).thenReturn(uow);

        final Field result = JOPAPersistenceProvider.getEntityField(instance, "transientFieldWithAnnotation");
        assertNull(result);
    }
}

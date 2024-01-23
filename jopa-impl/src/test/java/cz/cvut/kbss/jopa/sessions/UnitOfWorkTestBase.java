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
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassL;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.EntityManagerImpl;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.transactions.EntityTransaction;
import cz.cvut.kbss.jopa.utils.Configuration;
import org.mockito.Mock;

import java.lang.reflect.Field;
import java.net.URI;

import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

public abstract class UnitOfWorkTestBase {

    static final URI CONTEXT_URI = URI.create("http://testContext");

    protected Descriptor descriptor;

    protected OWLClassA entityA;
    OWLClassB entityB;
    OWLClassD entityD;
    OWLClassL entityL;

    @Mock
    protected MetamodelImpl metamodelMock;

    @Mock
    CacheManager cacheManagerMock;

    @Mock
    ConnectionWrapper storageMock;

    @Mock
    protected EntityManagerImpl emMock;

    @Mock
    protected EntityTransaction transactionMock;

    protected MetamodelMocks metamodelMocks;

    ServerSessionStub serverSessionStub;

    CloneBuilder cloneBuilder;

    protected UnitOfWorkImpl uow;

    protected void setUp() throws Exception {
        this.descriptor = new EntityDescriptor(CONTEXT_URI);
        this.serverSessionStub = spy(new ServerSessionStub(storageMock));
        when(serverSessionStub.getMetamodel()).thenReturn(metamodelMock);
        when(serverSessionStub.getLiveObjectCache()).thenReturn(cacheManagerMock);
        when(serverSessionStub.acquireConnection()).thenReturn(storageMock);
        when(emMock.getTransaction()).thenReturn(transactionMock);
        final Configuration config = new Configuration();
        when(emMock.getConfiguration()).thenReturn(config);
        this.metamodelMocks = new MetamodelMocks();
        metamodelMocks.setMocks(metamodelMock);
        uow = new UnitOfWorkImpl(serverSessionStub, config);
        uow.setEntityManager(emMock);
        final Field cbField = UnitOfWorkImpl.class.getDeclaredField("cloneBuilder");
        cbField.setAccessible(true);
        this.cloneBuilder = spy((CloneBuilder) cbField.get(uow));
        cbField.set(uow, cloneBuilder);
        initEntities();
    }

    private void initEntities() {
        this.entityA = Generators.generateOwlClassAInstance();
        this.entityB = new OWLClassB(Generators.createIndividualIdentifier());
        this.entityD = new OWLClassD(Generators.createIndividualIdentifier());
        entityD.setOwlClassA(entityA);
        this.entityL = new OWLClassL();
        entityL.setUri(Generators.createIndividualIdentifier());
    }
}

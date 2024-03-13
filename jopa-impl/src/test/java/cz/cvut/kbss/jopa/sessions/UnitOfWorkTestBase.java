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
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.transactions.EntityTransaction;
import org.mockito.Mock;

import java.lang.reflect.Field;
import java.net.URI;

import static org.mockito.Mockito.spy;

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
    ConnectionWrapper storageMock;

    @Mock
    protected EntityTransaction transactionMock;

    protected MetamodelMocks metamodelMocks;

    ServerSessionStub serverSessionStub;

    CloneBuilder cloneBuilder;

    protected AbstractUnitOfWork uow;

    protected void setUp() throws Exception {
        this.descriptor = new EntityDescriptor(CONTEXT_URI);
        this.serverSessionStub = spy(new ServerSessionStub(metamodelMock, storageMock));
        this.metamodelMocks = new MetamodelMocks();
        metamodelMocks.setMocks(metamodelMock);
        this.uow = initUnitOfWork();
        uow.begin();
        final Field cbField = AbstractUnitOfWork.class.getDeclaredField("cloneBuilder");
        cbField.setAccessible(true);
        this.cloneBuilder = spy((CloneBuilder) cbField.get(uow));
        cbField.set(uow, cloneBuilder);
        initEntities();
    }

    protected abstract AbstractUnitOfWork initUnitOfWork();

    private void initEntities() {
        this.entityA = Generators.generateOwlClassAInstance();
        this.entityB = new OWLClassB(Generators.createIndividualIdentifier());
        this.entityD = new OWLClassD(Generators.createIndividualIdentifier());
        entityD.setOwlClassA(entityA);
        this.entityL = new OWLClassL();
        entityL.setUri(Generators.createIndividualIdentifier());
    }
}

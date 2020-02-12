/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.sessions.CacheManager;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.mockito.Mock;

import java.net.URI;

abstract class InstanceLoaderTestBase {

    static final URI IDENTIFIER = Generators.createIndividualIdentifier();
    static final NamedResource INDIVIDUAL = NamedResource.create(IDENTIFIER);

    static Descriptor descriptor;
    static AxiomDescriptor axiomDescriptor;

    @Mock
    Connection connectionMock;

    @Mock
    MetamodelImpl metamodelMock;

    @Mock
    CacheManager cacheMock;

    @Mock
    AxiomDescriptorFactory descriptorFactoryMock;

    @Mock
    EntityConstructor entityConstructorMock;

    EntityInstanceLoader instanceLoader;

    static void staticSetup() {
        descriptor = new EntityDescriptor();
        axiomDescriptor = new AxiomDescriptor(NamedResource.create(IDENTIFIER));
    }
}

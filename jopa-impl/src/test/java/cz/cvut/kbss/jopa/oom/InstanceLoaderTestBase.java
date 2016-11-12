package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.junit.Rule;
import org.junit.rules.ExpectedException;
import org.mockito.Mock;

import java.net.URI;

public abstract class InstanceLoaderTestBase {

    static final URI ENTITY_PK = Generators.createIndividualIdentifier();
    static final NamedResource INDIVIDUAL = NamedResource.create(ENTITY_PK);

    static Descriptor descriptor;
    static AxiomDescriptor axiomDescriptor;

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Mock
    Connection connectionMock;

    @Mock
    MetamodelImpl metamodelMock;

    @Mock
    AxiomDescriptorFactory descriptorFactoryMock;

    @Mock
    EntityConstructor entityConstructorMock;

    EntityInstanceLoader instanceLoader;

    static void staticSetup() {
        descriptor = new EntityDescriptor();
        axiomDescriptor = new AxiomDescriptor(NamedResource.create(ENTITY_PK));
    }
}

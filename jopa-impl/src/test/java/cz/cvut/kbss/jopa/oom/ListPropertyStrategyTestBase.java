package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import org.mockito.Mock;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import static org.mockito.Mockito.when;

public class ListPropertyStrategyTestBase {

    protected static final URI PK = URI
            .create("http://krizik.felk.cvut.cz/ontologies/jopa/entityC");

    @Mock
    protected EntityMappingHelper mapperMock;

    @Mock
    protected CascadeResolver cascadeResolverMock;

    protected MetamodelMocks mocks;
    protected Descriptor descriptor;

    protected void setUp() throws Exception {
        this.mocks = new MetamodelMocks();
        when(mapperMock.getEntityType(OWLClassA.class)).thenReturn(mocks.forOwlClassA().entityType());
        this.descriptor = new EntityDescriptor();
    }

    protected static List<OWLClassA> generateList() {
        final List<OWLClassA> lst = new ArrayList<>();
        for (int i = 0; i < 10; i++) {
            final OWLClassA a = new OWLClassA();
            a.setUri(URI
                    .create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA_"
                            + i));
            lst.add(a);
        }
        return lst;
    }
}

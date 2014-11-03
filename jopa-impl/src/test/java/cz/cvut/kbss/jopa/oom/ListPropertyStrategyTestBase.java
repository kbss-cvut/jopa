package cz.cvut.kbss.jopa.oom;

import static org.mockito.Mockito.when;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import org.mockito.Mock;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassC;
import cz.cvut.kbss.jopa.test.utils.TestEnvironmentUtils;

public class ListPropertyStrategyTestBase {

	protected static final URI PK = URI
			.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityC");

	@Mock
	protected EntityType<OWLClassC> etC;

	@Mock
	protected ListAttribute simpleList;

	@Mock
	protected ListAttribute refList;

	@Mock
	protected Identifier idC;

	@Mock
	protected EntityType<OWLClassA> etA;

	@Mock
	protected Identifier idA;

	@Mock
	protected EntityMappingHelper mapperMock;

	@Mock
	protected CascadeResolver cascadeResolverMock;

	protected Descriptor descriptor;

	protected void setUp() throws Exception {
		TestEnvironmentUtils.initOWLClassCMocks(etC, simpleList, refList, idC);
		when(mapperMock.getEntityType(OWLClassA.class)).thenReturn(etA);
		when(etA.getIdentifier()).thenReturn(idA);
		when(idA.getJavaField()).thenReturn(
				OWLClassA.class.getDeclaredField("uri"));

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

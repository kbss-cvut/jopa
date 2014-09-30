package cz.cvut.kbss.jopa.oom;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.*;

import java.net.URI;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassH;

public class PersistCascadeResolverTest {

	private static final URI CONTEXT = URI
			.create("http://krizik.felk.cvut.cz/ontologies/contextOne");

	private static OWLClassA entityA;

	@Mock
	private EntityType<OWLClassA> etAMock;

	@Mock
	private Identifier idA;

	@Mock
	private ObjectOntologyMapperImpl mapperMock;

	private CascadeResolver resolver;

	@BeforeClass
	public static void setUpBeforeClass() {
		entityA = new OWLClassA();
		entityA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA"));
	}

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		when(etAMock.getIdentifier()).thenReturn(idA);
		when(idA.getJavaField()).thenReturn(OWLClassA.class.getDeclaredField("uri"));
		this.resolver = new PersistCascadeResolver(mapperMock);
	}

	@Test
	public void testResolveFieldCascadingCascadeAllNoContext() throws Exception {
		final FieldSpecification<?, ?> fieldSpecMock = mockFieldSpecForCascadeAll();
		resolver.resolveFieldCascading(fieldSpecMock, entityA, CONTEXT);
		verify(fieldSpecMock).getJavaField();
		verify(mapperMock, never()).registerPendingPersist(any(URI.class), any(Object.class),
				any(URI.class));
	}

	private FieldSpecification<?, ?> mockFieldSpecForCascadeAll() throws Exception {
		final FieldSpecification<?, ?> fs = mock(FieldSpecification.class);
		when(fs.getJavaField()).thenReturn(OWLClassH.getOwlClassAField());
		return fs;
	}

	@Test
	public void testResolveFieldCascadingPersistCascade() throws Exception {
		final FieldSpecification<?, ?> fsMock = mockFieldSpecForCascadePersist();
		resolver.resolveFieldCascading(fsMock, entityA, CONTEXT);
		verify(fsMock).getJavaField();
		verify(mapperMock, never()).registerPendingPersist(any(URI.class), any(Object.class),
				eq(CONTEXT));
	}

	private FieldSpecification<?, ?> mockFieldSpecForCascadePersist() throws Exception {
		final FieldSpecification<?, ?> fs = mock(FieldSpecification.class);
		when(fs.getJavaField()).thenReturn(PersistCascade.class.getDeclaredField("a"));
		return fs;
	}

	@Test
	public void testResolveFieldCascadingNoCascade() throws Exception {
		final FieldSpecification<?, ?> fsMock = mockFieldSpecForCascadeNone();
		when(mapperMock.getEntityType(OWLClassA.class)).thenReturn(etAMock);
		resolver.resolveFieldCascading(fsMock, entityA, CONTEXT);
		verify(mapperMock).registerPendingPersist(entityA.getUri(), entityA, CONTEXT);
	}

	private FieldSpecification<?, ?> mockFieldSpecForCascadeNone() throws Exception {
		final FieldSpecification<?, ?> fs = mock(FieldSpecification.class);
		when(fs.getJavaField()).thenReturn(OWLClassD.getOwlClassAField());
		return fs;
	}

	static final class PersistCascade {
		@Id
		private URI uri;
		@OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA", cascade = CascadeType.PERSIST)
		private OWLClassA a;

		public URI getUri() {
			return uri;
		}

		public void setUri(URI uri) {
			this.uri = uri;
		}

		public OWLClassA getA() {
			return a;
		}

		public void setA(OWLClassA a) {
			this.a = a;
		}
	}
}

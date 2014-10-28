package cz.cvut.kbss.ontodriver.sesame;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.model.ValueFactory;
import org.openrdf.repository.Repository;
import org.openrdf.repository.sail.SailRepository;
import org.openrdf.sail.memory.MemoryStore;

import cz.cvut.kbss.ontodriver.exceptions.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptorImpl;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

public class SimpleListHandlerTest {

	private static final NamedResource OWNER = NamedResource
			.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityC");
	private static final String LIST_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#C-hasSimpleSequence";
	private static final String NEXT_NODE_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#C-hasSimpleNext";

	private static ValueFactory vf;
	private static Repository repo;
	private static Resource owner;
	private static URI hasListProperty;
	private static URI nextNodeProperty;

	@Mock
	private Connector connector;

	private SimpleListDescriptor listDescriptor;

	private ListHandler<SimpleListDescriptor> handler;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		final MemoryStore mStore = new MemoryStore();
		repo = new SailRepository(mStore);
		vf = repo.getValueFactory();
		owner = vf.createURI(OWNER.toString());
		hasListProperty = vf.createURI(LIST_PROPERTY);
		nextNodeProperty = vf.createURI(NEXT_NODE_PROPERTY);
	}

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		final Assertion listProperty = Assertion.createObjectPropertyAssertion(
				java.net.URI.create(LIST_PROPERTY), false);
		final Assertion nextNodeProperty = Assertion.createObjectPropertyAssertion(
				java.net.URI.create(NEXT_NODE_PROPERTY), false);
		this.listDescriptor = new SimpleListDescriptorImpl(OWNER, listProperty, nextNodeProperty);

		this.handler = new SimpleListHandler(listDescriptor, connector, vf);
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		repo.shutDown();
	}

	@Test
	public void loadsEmptyListAndReturnsEmptyCollection() throws Exception {
		when(connector.findStatements(owner, hasListProperty, null, false, (URI[]) null))
				.thenReturn(Collections.<Statement> emptyList());
		final Collection<Axiom<?>> res = handler.loadList();
		assertNotNull(res);
		assertTrue(res.isEmpty());
		verify(connector, never()).findStatements(any(Resource.class), eq(nextNodeProperty),
				any(Value.class), any(Boolean.class), eq((URI[]) null));
	}

	@Test
	public void loadsSimpleList() throws Exception {
		final List<java.net.URI> simpleList = initSimpleList();
		final Map<java.net.URI, Statement> statements = initStatementsForList(simpleList);
		for (Entry<java.net.URI, Statement> e : statements.entrySet()) {
			final URI property = e.getKey().equals(OWNER.getIdentifier()) ? hasListProperty
					: nextNodeProperty;
			when(
					connector.findStatements(e.getValue().getSubject(), property, null, false,
							(URI[]) null)).thenReturn(Collections.singleton(e.getValue()));
		}
		final java.net.URI lastElem = simpleList.get(simpleList.size() - 1);
		when(
				connector.findStatements(vf.createURI(lastElem.toString()), nextNodeProperty, null,
						false, (URI[]) null)).thenReturn(Collections.<Statement> emptySet());

		final Collection<Axiom<?>> res = handler.loadList();
		verify(connector).findStatements(owner, hasListProperty, null, false, (URI[]) null);
		assertEquals(simpleList.size(), res.size());
		int i = 0;
		for (Axiom<?> ax : res) {
			assertEquals(simpleList.get(i), ax.getValue().getValue());
			i++;
		}
	}

	private List<java.net.URI> initSimpleList() {
		final List<java.net.URI> lst = new ArrayList<>();
		for (int i = 0; i < 5; i++) {
			lst.add(java.net.URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/elem" + i));
		}
		return lst;
	}

	private Map<java.net.URI, Statement> initStatementsForList(List<java.net.URI> simpleList) {
		final Map<java.net.URI, Statement> map = new HashMap<>(simpleList.size());
		java.net.URI uriSubject = OWNER.getIdentifier();
		Resource subject = owner;
		for (java.net.URI elem : simpleList) {
			Statement stmt;
			final Resource value = vf.createURI(elem.toString());
			final URI property = subject.equals(owner) ? hasListProperty : nextNodeProperty;
			stmt = vf.createStatement(subject, property, value);
			map.put(uriSubject, stmt);
			subject = value;
			uriSubject = elem;
		}
		return map;
	}

	@Test(expected = IntegrityConstraintViolatedException.class)
	public void throwsICViolationExceptionWhenMultipleHasListValuesFound() throws Exception {
		final Collection<Statement> stmts = new HashSet<>();
		stmts.add(mock(Statement.class));
		stmts.add(mock(Statement.class));
		when(connector.findStatements(owner, hasListProperty, null, false, (URI[]) null))
				.thenReturn(stmts);

		try {
			handler.loadList();
		} finally {
			verify(connector, never()).findStatements(any(Resource.class), eq(nextNodeProperty),
					any(Value.class), any(Boolean.class), any(URI[].class));
		}
	}

	@Test(expected = IntegrityConstraintViolatedException.class)
	public void throwsICViolationExceptionWhenMultipleNodeSuccessorsAreFound() throws Exception {
		final Collection<Statement> stmts = new HashSet<>();
		stmts.add(mock(Statement.class));
		stmts.add(mock(Statement.class));
		final Resource firstElem = vf
				.createURI("http://krizik.felk.cvut.cz/ontologies/jopa/firstElem");
		final Statement firstStmt = vf.createStatement(owner, hasListProperty, firstElem);

		when(connector.findStatements(owner, hasListProperty, null, false, (URI[]) null))
				.thenReturn(Collections.singleton(firstStmt));
		when(connector.findStatements(firstElem, nextNodeProperty, null, false, (URI[]) null))
				.thenReturn(stmts);

		try {
			handler.loadList();
		} finally {
			verify(connector).findStatements(owner, hasListProperty, null, false, (URI[]) null);
		}
	}

	@Test(expected = IntegrityConstraintViolatedException.class)
	public void throwsICViolationExceptionWhenLiteralIsFoundInList() throws Exception {
		final Resource firstElem = vf
				.createURI("http://krizik.felk.cvut.cz/ontologies/jopa/firstElem");
		final Statement firstStmt = vf.createStatement(owner, hasListProperty, firstElem);
		when(connector.findStatements(owner, hasListProperty, null, false, (URI[]) null))
				.thenReturn(Collections.singleton(firstStmt));
		final Statement nextStmt = vf.createStatement(firstElem, nextNodeProperty,
				vf.createLiteral(System.currentTimeMillis()));
		when(connector.findStatements(firstElem, nextNodeProperty, null, false, (URI[]) null))
				.thenReturn(Collections.singleton(nextStmt));

		try {
			handler.loadList();
		} finally {
			verify(connector).findStatements(owner, hasListProperty, null, false, (URI[]) null);
			verify(connector)
					.findStatements(firstElem, nextNodeProperty, null, false, (URI[]) null);
		}
	}
}

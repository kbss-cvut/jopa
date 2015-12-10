package cz.cvut.kbss.ontodriver.sesame;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;

import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptorImpl;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

@SuppressWarnings({ "unchecked" })
public class SimpleListHandlerTest extends ListHandlerTestBase {

	protected static final String LIST_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#C-hasSimpleSequence";
	protected static final String NEXT_NODE_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#C-hasSimpleNext";

	protected static URI hasSimpleListProperty;
	protected static URI nextNodeProperty;

	@Mock
	private Connector connector;

	private SimpleListDescriptor listDescriptor;

	private SimpleListHandler handler;

	private final Collection<Statement> added = new ArrayList<>();
	private final Collection<Statement> removed = new ArrayList<>();

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		init();
		hasSimpleListProperty = vf.createURI(LIST_PROPERTY);
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
		doAnswer(new Answer<Void>() {

			@Override
			public Void answer(InvocationOnMock invocation) throws Throwable {
				final Collection<Statement> arg = (Collection<Statement>) invocation.getArguments()[0];
				added.addAll(arg);
				return null;
			}
		}).when(connector).addStatements(any(Collection.class));
		doAnswer(new Answer<Void>() {

			@Override
			public Void answer(InvocationOnMock invocation) throws Throwable {
				final Collection<Statement> arg = (Collection<Statement>) invocation.getArguments()[0];
				removed.addAll(arg);
				return null;
			}
		}).when(connector).removeStatements(any(Collection.class));

		this.handler = new SimpleListHandler(connector, vf);
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		close();
	}

	@Test
	public void staticFactoryMethodForSimpleLists() throws Exception {
		final ListHandler<?, ?> h = ListHandler.createForSimpleList(connector, vf);
		assertNotNull(h);
		assertTrue(h instanceof SimpleListHandler);
	}

	@Test
	public void loadsEmptyListAndReturnsEmptyCollection() throws Exception {
		when(connector.findStatements(owner, hasSimpleListProperty, null, false, (URI[]) null))
				.thenReturn(Collections.<Statement> emptyList());
		final Collection<Axiom<NamedResource>> res = handler.loadList(listDescriptor);
		assertNotNull(res);
		assertTrue(res.isEmpty());
		verify(connector, never()).findStatements(any(Resource.class), eq(nextNodeProperty),
				any(Value.class), any(Boolean.class), eq((URI[]) null));
	}

	@Test
	public void loadsSimpleList() throws Exception {
		final List<NamedResource> simpleList = initList();
		initStatementsForList(simpleList);

		final Collection<Axiom<NamedResource>> res = handler.loadList(listDescriptor);
		verify(connector).findStatements(owner, hasSimpleListProperty, null, false, (URI[]) null);
		assertEquals(simpleList.size(), res.size());
		int i = 0;
		for (Axiom<?> ax : res) {
			assertEquals(simpleList.get(i), ax.getValue().getValue());
			i++;
		}
	}

	private List<Statement> initStatementsForList(List<NamedResource> simpleList)
			throws SesameDriverException {
		Resource subject = owner;
		final List<Statement> statements = new ArrayList<>(simpleList.size());
		for (NamedResource elem : simpleList) {
			Statement stmt;
			final Resource value = vf.createURI(elem.toString());
			final URI property = subject == owner ? hasSimpleListProperty : nextNodeProperty;
			stmt = vf.createStatement(subject, property, value);
			when(connector.findStatements(subject, property, null, false, (URI[]) null))
					.thenReturn(Collections.singleton(stmt));
			statements.add(stmt);
			subject = value;
		}
		return statements;
	}

	@Test(expected = IntegrityConstraintViolatedException.class)
	public void throwsICViolationExceptionWhenMultipleHasListValuesFound() throws Exception {
		final Collection<Statement> stmts = new HashSet<>();
		stmts.add(mock(Statement.class));
		stmts.add(mock(Statement.class));
		when(connector.findStatements(owner, hasSimpleListProperty, null, false, (URI[]) null))
				.thenReturn(stmts);

		try {
			handler.loadList(listDescriptor);
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
		final Statement firstStmt = vf.createStatement(owner, hasSimpleListProperty, firstElem);

		when(connector.findStatements(owner, hasSimpleListProperty, null, false, (URI[]) null))
				.thenReturn(Collections.singleton(firstStmt));
		when(connector.findStatements(firstElem, nextNodeProperty, null, false, (URI[]) null))
				.thenReturn(stmts);

		try {
			handler.loadList(listDescriptor);
		} finally {
			verify(connector).findStatements(owner, hasSimpleListProperty, null, false,
					(URI[]) null);
		}
	}

	@Test(expected = IntegrityConstraintViolatedException.class)
	public void throwsICViolationExceptionWhenLiteralIsFoundInList() throws Exception {
		final Resource firstElem = vf
				.createURI("http://krizik.felk.cvut.cz/ontologies/jopa/firstElem");
		final Statement firstStmt = vf.createStatement(owner, hasSimpleListProperty, firstElem);
		when(connector.findStatements(owner, hasSimpleListProperty, null, false, (URI[]) null))
				.thenReturn(Collections.singleton(firstStmt));
		final Statement nextStmt = vf.createStatement(firstElem, nextNodeProperty,
				vf.createLiteral(System.currentTimeMillis()));
		when(connector.findStatements(firstElem, nextNodeProperty, null, false, (URI[]) null))
				.thenReturn(Collections.singleton(nextStmt));

		try {
			handler.loadList(listDescriptor);
		} finally {
			verify(connector).findStatements(owner, hasSimpleListProperty, null, false,
					(URI[]) null);
			verify(connector)
					.findStatements(firstElem, nextNodeProperty, null, false, (URI[]) null);
		}
	}

	@Test
	public void persistsSimpleListWithSeveralValues() throws Exception {
		final SimpleListValueDescriptor listDescriptor = initValues(5);

		handler.persistList(listDescriptor);
		verify(connector).addStatements(any(Collection.class));
		for (Statement elem : added) {
			final NamedResource subject = NamedResource.create(SesameUtils.toJavaUri(elem
					.getSubject()));
			assertTrue(listDescriptor.getListOwner().equals(subject)
					|| listDescriptor.getValues().contains(subject));
		}
	}

	@Test
	public void persistsEmptySimpleList() throws Exception {
		final SimpleListValueDescriptor listDescriptor = spy(initValues(0));

		handler.persistList(listDescriptor);
		verify(listDescriptor).getValues();
		verify(connector, never()).addStatements(any(Collection.class));
	}

	private static SimpleListValueDescriptor initValues(int count) {
		final SimpleListValueDescriptor desc = new SimpleListValueDescriptor(OWNER,
				Assertion.createObjectPropertyAssertion(java.net.URI.create(LIST_PROPERTY), false),
				Assertion.createObjectPropertyAssertion(java.net.URI.create(NEXT_NODE_PROPERTY),
						false));
		for (int i = 0; i < count; i++) {
			desc.addValue(NamedResource
					.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA_" + i));
		}
		return desc;
	}

	@Test
	public void clearsListOnUpdateWhenDescriptorHasNoValues() throws Exception {
		final SimpleListValueDescriptor descriptor = initValues(0);
		// old list
		final List<NamedResource> simpleList = initList();
		final List<Statement> oldList = initStatementsForList(simpleList);

		handler.updateList(descriptor);
		verify(connector).removeStatements(oldList);
		verify(connector, never()).addStatements(any(Collection.class));
	}

	@Test
	public void insertsListOnUpdateWhenThereWereNoValuesBefore() throws Exception {
		final SimpleListValueDescriptor descriptor = initValues(5);
		when(
				connector.findStatements(owner, hasSimpleListProperty, null, descriptor
						.getListProperty().isInferred(), (URI[]) null)).thenReturn(
				Collections.<Statement> emptyList());

		handler.updateList(descriptor);
		verify(connector, never()).removeStatements(any(Collection.class));
		int i = 0;
		assertFalse(added.isEmpty());
		for (Statement stmt : added) {
			assertEquals(descriptor.getValues().get(i++).getIdentifier(),
					java.net.URI.create(stmt.getObject().stringValue()));
		}
	}

	@Test
	public void updateListAddsNewValuesToTheEnd() throws Exception {
		final SimpleListValueDescriptor descriptor = initValues(0);
		final SimpleListValueDescriptor tempDesc = initValues(8);
		final List<NamedResource> simpleList = initList();
		initStatementsForList(simpleList);
		// The original items
		for (NamedResource item : simpleList) {
			descriptor.addValue(item);
		}
		// Now add the new ones
		for (NamedResource r : tempDesc.getValues()) {
			descriptor.addValue(r);
		}

		handler.updateList(descriptor);
		verify(connector, never()).removeStatements(any(Collection.class));
		verify(connector, atLeast(1)).addStatements(any(Collection.class));
		assertEquals(tempDesc.getValues().size(), added.size());
		for (Statement stmt : added) {
			final java.net.URI u = java.net.URI.create(stmt.getObject().stringValue());
			assertTrue(tempDesc.getValues().contains(NamedResource.create(u)));
		}
	}

	@Test
	public void updateListRemovesSeveralElements() throws Exception {
		final SimpleListValueDescriptor descriptor = initValues(0);
		final List<NamedResource> simpleList = initList();
		initStatementsForList(simpleList);
		// Retain every even element, others will be removed
		int i = 0;
		final List<NamedResource> toRemove = new ArrayList<>();
		for (NamedResource item : simpleList) {
			if (i % 2 != 0) {
				toRemove.add(item);
			} else {
				descriptor.addValue(item);
			}
			i++;
		}

		handler.updateList(descriptor);
		for (NamedResource uri : toRemove) {
			boolean foundAsObject = false;
			boolean foundAsSubject = false;
			for (Statement rem : removed) {
				if (rem.getObject().stringValue().equals(uri.toString())) {
					foundAsObject = true;
				} else if (rem.getSubject().stringValue().equals(uri.toString())) {
					foundAsSubject = true;
				}
			}
			assertTrue(foundAsObject);
			assertTrue(foundAsSubject);
			for (Statement add : added) {
				if (add.getObject().stringValue().equals(uri.toString())) {
					fail("Found uri which shouln't have been added. " + uri);
				}
			}
			// Make sure that all the retained nodes were added
			for (int j = 1; j < descriptor.getValues().size(); j++) {
				final java.net.URI subject = descriptor.getValues().get(j - 1).getIdentifier();
				final java.net.URI object = descriptor.getValues().get(j).getIdentifier();
				boolean found = false;
				for (Statement add : added) {
					if (add.getSubject().stringValue().equals(subject.toString())
							&& add.getObject().stringValue().equals(object.toString())) {
						found = true;
						break;
					}
				}
				assertTrue(found);
			}
		}
	}
}

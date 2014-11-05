package cz.cvut.kbss.ontodriver.sesame;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;

import cz.cvut.kbss.ontodriver.exceptions.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptorImpl;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

@SuppressWarnings({ "unchecked", "rawtypes" })
public class SimpleListHandlerTest extends ListHandlerTestBase {

	protected static final String LIST_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#C-hasSimpleSequence";
	protected static final String NEXT_NODE_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#C-hasSimpleNext";

	protected static URI hasSimpleListProperty;
	protected static URI nextNodeProperty;

	@Mock
	private Connector connector;

	private SimpleListDescriptor listDescriptor;

	private SimpleListHandler handler;

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
		final Collection<Axiom<?>> res = handler.loadList(listDescriptor);
		assertNotNull(res);
		assertTrue(res.isEmpty());
		verify(connector, never()).findStatements(any(Resource.class), eq(nextNodeProperty),
				any(Value.class), any(Boolean.class), eq((URI[]) null));
	}

	@Test
	public void loadsSimpleList() throws Exception {
		final List<java.net.URI> simpleList = initList();
		initStatementsForList(simpleList);

		final Collection<Axiom<?>> res = handler.loadList(listDescriptor);
		verify(connector).findStatements(owner, hasSimpleListProperty, null, false, (URI[]) null);
		assertEquals(simpleList.size(), res.size());
		int i = 0;
		for (Axiom<?> ax : res) {
			assertEquals(simpleList.get(i), ax.getValue().getValue());
			i++;
		}
	}

	private List<Statement> initStatementsForList(List<java.net.URI> simpleList)
			throws SesameDriverException {
		Resource subject = owner;
		final List<Statement> statements = new ArrayList<>(simpleList.size());
		for (java.net.URI elem : simpleList) {
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
		final ArgumentCaptor<Collection> captor = ArgumentCaptor.forClass(Collection.class);
		verify(connector).addStatements(captor.capture());
		final Collection<?> args = captor.getValue();
		for (Object elem : args) {
			assert (elem instanceof Statement);
			final Statement stmt = (Statement) elem;
			final NamedResource subject = NamedResource.create(SesameUtils.toJavaUri(stmt
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
		final List<java.net.URI> simpleList = initList();
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
		final ArgumentCaptor<Collection> captor = ArgumentCaptor.forClass(Collection.class);
		verify(connector).addStatements(captor.capture());
		int i = 0;
		for (Object item : captor.getValue()) {
			final Statement stmt = (Statement) item;
			assertEquals(descriptor.getValues().get(i++).getIdentifier(),
					java.net.URI.create(stmt.getObject().stringValue()));
		}
	}

	@Test
	public void updateListReplacesSeveralElements() throws Exception {
		final SimpleListValueDescriptor descriptor = initValues(8);
		final List<java.net.URI> simpleList = new ArrayList<>();
		for (NamedResource item : descriptor.getValues()) {
			simpleList.add(item.getIdentifier());
		}
		final int iOne = 0;
		final java.net.URI diffOne = java.net.URI.create("http://krizik.felk.cvut.cz/modified#One");
		simpleList.set(iOne, diffOne);
		final java.net.URI diffTwo = java.net.URI.create("http://krizik.felk.cvut.cz/modified#Two");
		final int iTwo = 7;
		simpleList.set(iTwo, diffTwo);
		final List<Statement> oldList = initStatementsForList(simpleList);

		handler.updateList(descriptor);
		final Collection<Statement> toRemove = Arrays.asList(oldList.get(iOne), oldList.get(iTwo));
		final ArgumentCaptor<Collection> captorRemove = ArgumentCaptor.forClass(Collection.class);
		verify(connector).removeStatements(captorRemove.capture());
		assertEquals(toRemove, captorRemove.getValue());
		final ArgumentCaptor<Collection> captorAdd = ArgumentCaptor.forClass(Collection.class);
		verify(connector).addStatements(captorAdd.capture());
		assertEquals(2, captorAdd.getValue().size());
		for (Object item : captorAdd.getValue()) {
			final Statement stmt = (Statement) item;
			final java.net.URI u = java.net.URI.create(stmt.getObject().stringValue());
			assertTrue(u.equals(diffOne) || u.equals(diffTwo));
		}
	}

	@Test
	public void updateListAddsNewValuesToTheEnd() throws Exception {
		final SimpleListValueDescriptor descriptor = initValues(0);
		final SimpleListValueDescriptor tempDesc = initValues(8);
		final List<java.net.URI> simpleList = initList();
		initStatementsForList(simpleList);
		// The original items
		for (java.net.URI item : simpleList) {
			descriptor.addValue(NamedResource.create(item));
		}
		// Now add the new ones
		for (NamedResource r : tempDesc.getValues()) {
			descriptor.addValue(r);
		}

		handler.updateList(descriptor);
		verify(connector, never()).removeStatements(any(Collection.class));
		final ArgumentCaptor<Collection> captorAdd = ArgumentCaptor.forClass(Collection.class);
		verify(connector).addStatements(captorAdd.capture());
		assertEquals(tempDesc.getValues().size(), captorAdd.getValue().size());
		for (Object item : captorAdd.getValue()) {
			final Statement stmt = (Statement) item;
			final java.net.URI u = java.net.URI.create(stmt.getObject().stringValue());
			assertTrue(tempDesc.getValues().contains(NamedResource.create(u)));
		}
	}

	@Test
	public void updateListRemovesSeveralElements() throws Exception {
		final SimpleListValueDescriptor descriptor = initValues(0);
		final List<java.net.URI> simpleList = initList();
		initStatementsForList(simpleList);
		// Retain every even element, others will be removed
		int i = 0;
		final List<java.net.URI> toRemove = new ArrayList<>();
		for (java.net.URI item : simpleList) {
			if (i % 2 != 0) {
				toRemove.add(item);
			} else {
				descriptor.addValue(NamedResource.create(item));
			}
		}

		handler.updateList(descriptor);
		verify(connector, never()).addStatements(any(Collection.class));
		final ArgumentCaptor<Collection> captorRemove = ArgumentCaptor.forClass(Collection.class);
		verify(connector).removeStatements(captorRemove.capture());
		assertEquals(toRemove.size(), captorRemove.getValue().size());
		for (Object item : captorRemove.getValue()) {
			final Statement stmt = (Statement) item;
			final java.net.URI u = java.net.URI.create(stmt.getObject().stringValue());
			assertTrue(toRemove.contains(u));
		}
	}
}

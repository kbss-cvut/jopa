package cz.cvut.kbss.ontodriver.sesame;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Random;

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

import cz.cvut.kbss.ontodriver.exceptions.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListDescriptorImpl;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;

public class ReferencedListHandlerTest extends ListHandlerTestBase {

	private static final String NODE_CONTENT_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#C-hasContent";
	private static URI nodeContentProperty;

	private ReferencedListDescriptor listDescriptor;

	private ReferencedListHandler handler;

	@Mock
	private Connector connector;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		init();
		nodeContentProperty = vf.createURI(NODE_CONTENT_PROPERTY);
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		close();
	}

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		final Assertion listProperty = Assertion.createObjectPropertyAssertion(
				java.net.URI.create(LIST_PROPERTY), false);
		final Assertion nextNodeProperty = Assertion.createObjectPropertyAssertion(
				java.net.URI.create(NEXT_NODE_PROPERTY), false);
		final Assertion nodeContentProperty = Assertion.createObjectPropertyAssertion(
				java.net.URI.create(NODE_CONTENT_PROPERTY), false);
		this.listDescriptor = new ReferencedListDescriptorImpl(OWNER, listProperty,
				nextNodeProperty, nodeContentProperty);

		this.handler = new ReferencedListHandler(connector, vf);
	}

	@Test
	public void loadsEmptyListAndReturnsEmptyCollection() throws Exception {
		when(connector.findStatements(owner, hasListProperty, null, false, (URI[]) null))
				.thenReturn(Collections.<Statement> emptyList());
		final Collection<Axiom<?>> res = handler.loadList(listDescriptor);
		assertNotNull(res);
		assertTrue(res.isEmpty());
		verify(connector, never()).findStatements(any(Resource.class), eq(nextNodeProperty),
				any(Value.class), any(Boolean.class), eq((URI[]) null));
	}

	@Test
	public void loadsReferencedList() throws Exception {
		final List<java.net.URI> refList = initList();
		final List<java.net.URI> listNodes = initListNodes(refList);
		initStatementsForList(listNodes, refList);
		final Collection<Axiom<?>> res = handler.loadList(listDescriptor);
		assertEquals(refList.size(), res.size());
		for (Axiom<?> a : res) {
			assertTrue(refList.contains(a.getValue().getValue()));
		}
	}

	private List<java.net.URI> initListNodes(List<?> content) {
		final List<java.net.URI> nodes = new ArrayList<>();
		for (int i = 0; i < content.size(); i++) {
			nodes.add(java.net.URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/SEQ_" + i));
		}
		return nodes;
	}

	private void initStatementsForList(List<java.net.URI> nodes, List<java.net.URI> values)
			throws Exception {
		int i = 0;
		Resource prev = owner;
		for (java.net.URI item : nodes) {
			final URI itemUri = vf.createURI(item.toString());
			if (i == 0) {
				when(
						connector.findStatements(eq(prev), eq(hasListProperty), eq((Value) null),
								anyBoolean(), eq((URI[]) null))).thenReturn(
						Collections.singleton(vf.createStatement(prev, hasListProperty, itemUri)));
			} else {
				when(
						connector.findStatements(eq(prev), eq(nextNodeProperty), eq((Value) null),
								anyBoolean(), eq((URI[]) null))).thenReturn(
						Collections.singleton(vf.createStatement(prev, nextNodeProperty, itemUri)));
			}
			when(
					connector.findStatements(eq(itemUri), eq(nodeContentProperty),
							eq((Value) null), anyBoolean(), eq((URI[]) null))).thenReturn(
					Collections.singleton(vf.createStatement(owner, nodeContentProperty,
							vf.createURI(values.get(i).toString()))));
			prev = itemUri;
			i++;
		}
	}

	@Test(expected = IntegrityConstraintViolatedException.class)
	public void throwsICViolationWhenThereIsNoContentInHeadNode() throws Exception {
		final URI headNode = vf.createURI("http://krizik.felk.cvut.cz/ontologies/jopa/SEQ0");
		when(
				connector.findStatements(eq(owner), eq(hasListProperty), eq((Value) null),
						anyBoolean(), eq((URI[]) null))).thenReturn(
				Collections.singleton(vf.createStatement(owner, hasListProperty, headNode)));
		when(
				connector.findStatements(eq(headNode), eq(nodeContentProperty), eq((Value) null),
						anyBoolean(), eq((URI[]) null))).thenReturn(
				Collections.<Statement> emptyList());
		try {
			final Collection<Axiom<?>> res = handler.loadList(listDescriptor);
			assert res == null;
			fail("This line should not have been reached.");
		} finally {
			verify(connector, never()).findStatements(any(Resource.class), eq(nextNodeProperty),
					any(Value.class), anyBoolean(), any(URI[].class));
		}
	}

	@Test(expected = IntegrityConstraintViolatedException.class)
	public void throwsICViolationWhenThereIsNoContentInSomeListNode() throws Exception {
		final List<java.net.URI> refList = initList();
		final List<java.net.URI> listNodes = initListNodes(refList);
		initStatementsForList(listNodes, refList);
		final Resource elem = selectRandomNode(listNodes);
		when(
				connector.findStatements(eq(elem), eq(nodeContentProperty), eq((Value) null),
						anyBoolean(), eq((URI[]) null))).thenReturn(
				Collections.<Statement> emptyList());
		final Collection<Axiom<?>> res = handler.loadList(listDescriptor);
		assert res == null;
	}

	private Resource selectRandomNode(List<java.net.URI> nodes) {
		// Select a random index, but it shouldn't be 0 (it would be the head),
		// so add 1
		final int rand = new Random().nextInt(nodes.size() - 1) + 1;
		final Resource elem = vf.createURI(nodes.get(rand).toString());
		return elem;
	}

	@Test(expected = IntegrityConstraintViolatedException.class)
	public void throwsICViolationWhenThereAreMutlipleSuccessorsForNode() throws Exception {
		final List<java.net.URI> refList = initList();
		final List<java.net.URI> listNodes = initListNodes(refList);
		initStatementsForList(listNodes, refList);
		final Resource node = selectRandomNode(listNodes);
		final List<Statement> stmts = Arrays.asList(mock(Statement.class), mock(Statement.class));
		when(
				connector.findStatements(eq(node), eq(nextNodeProperty), eq((Value) null),
						anyBoolean(), eq((URI[]) null))).thenReturn(stmts);
		final Collection<Axiom<?>> res = handler.loadList(listDescriptor);
		assert res == null;
	}
}

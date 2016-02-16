/**
 * Copyright (C) 2011 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.IdentifierGenerationException;
import cz.cvut.kbss.ontodriver.model.*;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.RDF;
import org.openrdf.repository.Repository;
import org.openrdf.repository.sail.SailRepository;
import org.openrdf.sail.memory.MemoryStore;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.*;

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.*;

public class SesameAdapterTest {

	private static final String LANGUAGE = "en";
	private static final NamedResource SUBJECT = NamedResource
			.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityX");

	private static ValueFactory vf;
	private static Repository repo;
	private static org.openrdf.model.URI subjectUri;

	@Mock
	private Connector connectorMock;

	private SesameAdapter adapter;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		final MemoryStore mStore = new MemoryStore();
		repo = new SailRepository(mStore);
		repo.initialize();
		vf = repo.getValueFactory();
		subjectUri = vf.createURI(SUBJECT.getIdentifier().toString());
	}

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		when(connectorMock.getValueFactory()).thenReturn(vf);
		this.adapter = new SesameAdapter(connectorMock, Collections.singletonMap(
				OntoDriverProperties.ONTOLOGY_LANGUAGE, LANGUAGE));

	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		repo.shutDown();
	}

	@Test
	public void testSesameAdapter() throws Exception {
		this.adapter = new SesameAdapter(connectorMock, Collections.<String, String> emptyMap());
		final Field langField = SesameAdapter.class.getDeclaredField("language");
		langField.setAccessible(true);
		final String lang = (String) langField.get(adapter);
		assertNull(lang);
	}

	@Test
	public void testGetContexts() throws Exception {
		final List<Resource> contexts = new ArrayList<>();
		for (int i = 0; i < 5; i++) {
			contexts.add(vf.createURI("http://krizik.felk.cvut.cz/ontologies/context" + i));
		}
		when(connectorMock.getContexts()).thenReturn(contexts);
		final List<URI> res = adapter.getContexts();
		assertEquals(contexts.size(), res.size());
		for (int i = 0; i < contexts.size(); i++) {
			assertEquals(contexts.get(i).stringValue(), res.get(i).toString());
		}
	}

	@Test
	public void testGetContextsWithBNodes() throws Exception {
		final List<Resource> contexts = new ArrayList<>();
		int bnodes = 0;
		for (int i = 0; i < 5; i++) {
			if (i % 2 == 1) {
				contexts.add(vf.createBNode());
				bnodes++;
			} else {
				contexts.add(vf.createURI("http://krizik.felk.cvut.cz/ontologies/context" + i));
			}
		}
		when(connectorMock.getContexts()).thenReturn(contexts);
		final List<URI> res = adapter.getContexts();
		assertEquals(contexts.size() - bnodes, res.size());
	}

	@Test
	public void testClose() throws Exception {
		assertTrue(adapter.isOpen());
		adapter.close();
		verify(connectorMock).close();
		assertFalse(adapter.isOpen());
	}

	@Test
	public void testPersistDataPropertiesNoContext() throws Exception {
		final AxiomValueDescriptor ad = new AxiomValueDescriptor(SUBJECT);
		ad.addAssertionValue(
				Assertion.createClassAssertion(false),
				new Value<URI>(URI
						.create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")));
		ad.addAssertionValue(Assertion.createDataPropertyAssertion(URI
				.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute"),
				false), new Value<String>("StringValue"));
		adapter.persist(ad);
		final ArgumentCaptor<Collection> captor = ArgumentCaptor.forClass(Collection.class);
		verify(connectorMock).addStatements(captor.capture());
		final Collection<?> res = captor.getValue();
		assertEquals(2, res.size());
		assertTrue(statementsCorrespondToAxiomDescriptor(ad, (Collection<Statement>) res));
	}

	private boolean statementsCorrespondToAxiomDescriptor(AxiomValueDescriptor ad,
			Collection<Statement> statements) {
		for (Assertion as : ad.getAssertions()) {
			final String strIdentifier = as.getIdentifier().toString();
			for (Value<?> val : ad.getAssertionValues(as)) {
				boolean valueFound = false;
				for (Statement stmt : statements) {
					if (stmt.getPredicate().stringValue().equals(strIdentifier)
							&& stmt.getObject().stringValue().equals(val.getValue().toString())) {
						valueFound = true;
						assertEquals(ad.getSubject().getIdentifier().toString(), stmt.getSubject()
								.stringValue());
						if (ad.getAssertionContext(as) == null) {
							assertNull(stmt.getContext());
						} else if (stmt.getContext() == null) {
							assertNull(ad.getAssertionContext(as));
						} else {
							assertEquals(ad.getAssertionContext(as).toString(), stmt.getContext()
									.stringValue());
						}
						break;
					}
				}
				if (!valueFound) {
					return false;
				}
			}
		}
		return true;
	}

	@Test
	public void testPersistEntityWithTypesDataPropertyInContext() throws Exception {
		final AxiomValueDescriptor ad = new AxiomValueDescriptor(SUBJECT);
		ad.addAssertionValue(
				Assertion.createClassAssertion(false),
				new Value<URI>(URI
						.create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")));
		ad.addAssertionValue(
				Assertion.createClassAssertion(false),
				new Value<URI>(URI
						.create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassC")));
		ad.addAssertionValue(
				Assertion.createClassAssertion(false),
				new Value<URI>(URI
						.create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassD")));
		final Assertion dataAssertion = Assertion.createDataPropertyAssertion(URI
				.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute"),
				false);
		ad.addAssertionValue(dataAssertion, new Value<String>("StringValue"));
		ad.setAssertionContext(dataAssertion,
				URI.create("http://krizik.felk.cvut.cz/ontologies/contextOne"));
		adapter.persist(ad);
		final ArgumentCaptor<Collection> captor = ArgumentCaptor.forClass(Collection.class);
		verify(connectorMock).addStatements(captor.capture());
		final Collection<?> res = captor.getValue();
		assertEquals(4, res.size());
		assertTrue(statementsCorrespondToAxiomDescriptor(ad, (Collection<Statement>) res));
	}

	@Test
	public void testPersistEntityWithObjectPropertyMultipleValues() throws Exception {
		final AxiomValueDescriptor ad = new AxiomValueDescriptor(SUBJECT);
		ad.addAssertionValue(
				Assertion.createClassAssertion(false),
				new Value<URI>(URI
						.create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")));
		final Assertion objectAssertion = Assertion.createObjectPropertyAssertion(
				URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-owlclassY"),
				false);
		ad.addAssertionValue(objectAssertion,
				new Value<URI>(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityY")));
		ad.addAssertionValue(objectAssertion,
				new Value<URI>(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityYY")));
		ad.addAssertionValue(objectAssertion,
				new Value<URI>(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityYYY")));
		ad.setSubjectContext(URI.create("http://krizik.felk.cvut.cz/ontologies/contextOne"));
		adapter.persist(ad);
		final ArgumentCaptor<Collection> captor = ArgumentCaptor.forClass(Collection.class);
		verify(connectorMock).addStatements(captor.capture());
		final Collection<?> res = captor.getValue();
		assertEquals(4, res.size());
		assertTrue(statementsCorrespondToAxiomDescriptor(ad, (Collection<Statement>) res));
	}

	@Test
	public void testPersistEntityWithProperties() throws Exception {
		final AxiomValueDescriptor ad = new AxiomValueDescriptor(SUBJECT);
		ad.addAssertionValue(
				Assertion.createClassAssertion(false),
				new Value<URI>(URI
						.create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")));
		final Assertion objectAssertion = Assertion.createPropertyAssertion(
				URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-owlclassY"),
				false);
		ad.addAssertionValue(objectAssertion,
				new Value<URI>(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityY")));
		final Assertion dataAssertion = Assertion.createPropertyAssertion(URI
				.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute"),
				false);
		ad.addAssertionValue(dataAssertion, new Value<String>("stringValue"));
		adapter.persist(ad);
		final ArgumentCaptor<Collection> captor = ArgumentCaptor.forClass(Collection.class);
		verify(connectorMock).addStatements(captor.capture());
		final Collection<?> res = captor.getValue();
		assertEquals(3, res.size());
		assertTrue(statementsCorrespondToAxiomDescriptor(ad, (Collection<Statement>) res));
	}

	@Test
	public void testPersistEntityWithAnnotationPropertyAndDifferentSubjectAndPropertyContexts()
			throws Exception {
		final URI subjectCtx = URI.create("http://krizik.felk.cvut.cz/ontologies/contextOne");
		final URI propertyCtx = URI.create("http://krizik.felk.cvut.cz/ontologies/contextTwo");
		final AxiomValueDescriptor ad = new AxiomValueDescriptor(SUBJECT);
		ad.setSubjectContext(subjectCtx);
		ad.addAssertionValue(
				Assertion.createClassAssertion(false),
				new Value<URI>(URI
						.create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")));
		final Assertion dataAssertion = Assertion.createAnnotationPropertyAssertion(
				URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-annotation"),
				false);
		ad.addAssertionValue(dataAssertion, new Value<String>("StringValue"));
		ad.setAssertionContext(dataAssertion, propertyCtx);
		adapter.persist(ad);
		final ArgumentCaptor<Collection> captor = ArgumentCaptor.forClass(Collection.class);
		verify(connectorMock).addStatements(captor.capture());
		final Collection<?> res = captor.getValue();
		assertEquals(2, res.size());
		assertTrue(statementsCorrespondToAxiomDescriptor(ad, (Collection<Statement>) res));
		// This checks that the contexts are set correctly
		for (Object s : res) {
			final Statement stmt = (Statement) s;
			if (stmt.getPredicate().stringValue().equals(dataAssertion.getIdentifier().toString())) {
				assertEquals(propertyCtx.toString(), stmt.getContext().stringValue());
			} else {
				assertEquals(subjectCtx.toString(), stmt.getContext().stringValue());
			}
		}
	}

	@Test
	public void testFindEntityWithDataProperty() throws Exception {
		final AxiomDescriptor desc = new AxiomDescriptor(SUBJECT);
		desc.addAssertion(Assertion.createClassAssertion(false));
		final String property = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute";
		desc.addAssertion(Assertion.createDataPropertyAssertion(URI.create(property), false));
		final Map<Assertion, Statement> statements = initStatementsForDescriptor(desc);
		for (Assertion as : statements.keySet()) {
			final org.openrdf.model.URI predicate = vf.createURI(as.getIdentifier().toString());
			when(
					connectorMock.findStatements(subjectUri, predicate, null, as.isInferred())).thenReturn(
					Collections.singletonList(statements.get(as)));
		}
		final Collection<Axiom<?>> res = adapter.find(desc);
		verify(connectorMock, times(2)).findStatements(any(Resource.class),
				any(org.openrdf.model.URI.class), any(org.openrdf.model.Value.class), anyBoolean());
		assertEquals(statements.size(), res.size());
		for (Axiom<?> ax : res) {
			assertTrue(statements.containsKey(ax.getAssertion()));
			assertEquals(statements.get(ax.getAssertion()).getObject().stringValue(), ax.getValue()
					.getValue().toString());
		}
	}

	private Map<Assertion, Statement> initStatementsForDescriptor(AxiomDescriptor desc) {
		int opCounter = 0;
		int dpCounter = 0;
		int apCounter = 0;
		final Map<Assertion, Statement> lst = new HashMap<>();
		final Resource subject = vf.createURI(desc.getSubject().getIdentifier().toString());
		for (Assertion as : desc.getAssertions()) {
			final org.openrdf.model.URI property = vf.createURI(as.getIdentifier().toString());
			org.openrdf.model.Value val = null;
			switch (as.getType()) {
			case ANNOTATION_PROPERTY:
				val = vf.createLiteral("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#annotationPropertyValue"
						+ apCounter++);
				break;
			case CLASS:
				val = vf.createURI("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA");
				break;
			case DATA_PROPERTY:
				val = vf.createLiteral("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#dataPropertyValue"
						+ dpCounter++);
				break;
			case OBJECT_PROPERTY:
				val = vf.createURI("http://krizik.felk.cvut.cz/ontologies/jopa/entities#entityValue"
						+ opCounter++);
				break;
			default:
				break;
			}
			lst.put(as, vf.createStatement(subject, property, val));
		}
		return lst;
	}

	@Test
	public void testFindEntityWithObjectPropertyAndInferredAnnotationProperty() throws Exception {
		final AxiomDescriptor desc = new AxiomDescriptor(SUBJECT);
		desc.addAssertion(Assertion.createClassAssertion(false));
		final String anProperty = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-annotationAttribute";
		desc.addAssertion(Assertion.createAnnotationPropertyAssertion(URI.create(anProperty), true));
		final String obProperty = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-refToA";
		desc.addAssertion(Assertion.createObjectPropertyAssertion(URI.create(obProperty), false));
		final Map<Assertion, Statement> statements = initStatementsForDescriptor(desc);
		for (Assertion as : statements.keySet()) {
			final org.openrdf.model.URI predicate = vf.createURI(as.getIdentifier().toString());
			when(
					connectorMock.findStatements(subjectUri, predicate, null, as.isInferred())).thenReturn(
					Collections.singletonList(statements.get(as)));
		}
		final Collection<Axiom<?>> res = adapter.find(desc);
		verify(connectorMock, times(3)).findStatements(any(Resource.class),
				any(org.openrdf.model.URI.class), any(org.openrdf.model.Value.class), anyBoolean());
		verify(connectorMock, times(1)).findStatements(eq(subjectUri),
				eq(vf.createURI(anProperty)), (org.openrdf.model.Value) eq(null), eq(true));
		assertEquals(statements.size(), res.size());
		for (Axiom<?> ax : res) {
			assertTrue(statements.containsKey(ax.getAssertion()));
			assertEquals(statements.get(ax.getAssertion()).getObject().stringValue(), ax.getValue()
					.getValue().toString());
		}
	}

	@Test
	public void testFindEntityWithTypesAndSubjectContext() throws Exception {
		final AxiomDescriptor desc = new AxiomDescriptor(SUBJECT);
		final URI subjectCtx = URI.create("http://krizik.felk.cvut.cz/ontologies/contextOne");
		desc.addAssertion(Assertion.createClassAssertion(false));
		desc.setSubjectContext(subjectCtx);
		final List<Statement> statements = new ArrayList<>();
		final org.openrdf.model.URI typeProperty = vf.createURI(Assertion
				.createClassAssertion(false).getIdentifier().toString());
		final int count = 10;
		final Set<String> types = new HashSet<>();
		for (int i = 0; i < count; i++) {
			final org.openrdf.model.URI type = vf
					.createURI("http://krizik.felk.cvut.cz/ontologies/jopa/types#Type" + i);
			statements.add(vf.createStatement(subjectUri, typeProperty, type));
			types.add(type.stringValue());
		}
		when(
				connectorMock.findStatements(subjectUri, typeProperty, null, false,
						vf.createURI(subjectCtx.toString()))).thenReturn(statements);
		final Collection<Axiom<?>> res = adapter.find(desc);
		assertEquals(statements.size(), res.size());
		for (Axiom<?> ax : res) {
			assertTrue(types.contains(ax.getValue().getValue().toString()));
		}
	}

	@Test
	public void testFindEntityWithProperties() throws Exception {
		final AxiomDescriptor desc = new AxiomDescriptor(SUBJECT);
		desc.addAssertion(Assertion.createUnspecifiedPropertyAssertion(false));
		final String propertyOne = "http://krizik.felk.cvut.cz/ontologies/jopa/properties#objectProperty";
		final String propertyTwo = "http://krizik.felk.cvut.cz/ontologies/jopa/properties#dataProperty";
        desc.addAssertion(Assertion.createDataPropertyAssertion(URI.create(propertyTwo), false));
		final Map<Assertion, List<Statement>> statements = new HashMap<>();
		final Collection<Statement> stmts = new ArrayList<>();
		final Assertion asOne = Assertion.createPropertyAssertion(URI.create(propertyOne), false);
		statements.put(asOne, createStatementsForProperty(propertyOne, true));
		final Assertion asTwo = Assertion.createPropertyAssertion(URI.create(propertyTwo), false);
		statements.put(asTwo, createStatementsForProperty(propertyTwo, false));
		for (Assertion a : statements.keySet()) {
			stmts.addAll(statements.get(a));
		}
		when(
				connectorMock.findStatements(subjectUri, null, null, false)).thenReturn(stmts);
        when(connectorMock.findStatements(subjectUri, vf.createURI(propertyOne), null, false)).thenReturn(statements.get(asOne));
        when(connectorMock.findStatements(subjectUri, vf.createURI(propertyTwo), null, false)).thenReturn(statements.get(asTwo));

		final Collection<Axiom<?>> res = adapter.find(desc);
		verify(connectorMock).findStatements(subjectUri, null, null, false);
        verify(connectorMock, never()).findStatements(eq(subjectUri), eq(vf.createURI(propertyOne)),
                any(org.openrdf.model.Value.class), anyBoolean());
		verifyReturnedAxioms(stmts, res);
	}

    private List<Statement> createStatementsForProperty(String property, boolean objectProperty) {
        final List<Statement> res = new ArrayList<>();
        int cnt = 3;
        if (objectProperty) {
            for (int i = 0; i < cnt; i++) {
                res.add(vf.createStatement(subjectUri, vf.createURI(property), vf.createURI("http://krizik.felk.cvut.cz/jopa#entity" + i)));
            }
        } else {
            for (int i = 0; i < cnt; i++) {
                res.add(vf.createStatement(subjectUri, vf.createURI(property), vf.createLiteral(i)));
            }
        }
        return res;
    }

    private void verifyReturnedAxioms(Collection<Statement> expected, Collection<Axiom<?>> actual) {
        assertEquals(expected.size(), actual.size());
        boolean found;
        for (Axiom<?> ax : actual) {
            found = false;
            for (Statement s : expected) {
                if (s.getPredicate().toString().equals(ax.getAssertion().getIdentifier().toString())
                        && s.getObject().stringValue().equals(ax.getValue().stringValue())) {
                    found = true;
                    break;
                }
            }
            assertTrue(found);
        }
    }

    @Test
    public void testFindEntityWithPropertiesAndInferredDataProperty() throws Exception {
        final AxiomDescriptor desc = new AxiomDescriptor(SUBJECT);
        desc.addAssertion(Assertion.createUnspecifiedPropertyAssertion(false));
        final String propertyOne = "http://krizik.felk.cvut.cz/ontologies/jopa/properties#objectProperty";
        final String propertyTwo = "http://krizik.felk.cvut.cz/ontologies/jopa/properties#dataProperty";
        desc.addAssertion(Assertion.createDataPropertyAssertion(URI.create(propertyTwo), true));
        final Map<Assertion, List<Statement>> statements = new HashMap<>();
        final Collection<Statement> stmts = new ArrayList<>();
        final Assertion asOne = Assertion.createPropertyAssertion(URI.create(propertyOne), false);
        statements.put(asOne, createStatementsForProperty(propertyOne, true));
        final Assertion asTwo = Assertion.createDataPropertyAssertion(URI.create(propertyTwo), false);
        statements.put(asTwo, createStatementsForProperty(propertyTwo, false));
        for (Assertion a : statements.keySet()) {
            stmts.addAll(statements.get(a));
        }
        final Collection<Statement> inferred = new ArrayList<>();
        inferred.add(vf.createStatement(subjectUri, vf.createURI(propertyTwo), vf.createLiteral(true)));
        statements.get(asTwo).addAll(inferred);
        when(
                connectorMock.findStatements(subjectUri, null, null, false)).thenReturn(stmts);
        when(connectorMock.findStatements(subjectUri, vf.createURI(propertyOne), null, false)).thenReturn(statements.get(asOne));
        when(connectorMock.findStatements(subjectUri, vf.createURI(propertyTwo), null, true)).thenReturn(statements.get(asTwo));

        final Collection<Axiom<?>> res = adapter.find(desc);
        verify(connectorMock).findStatements(subjectUri, null, null, false);
        verify(connectorMock).findStatements(subjectUri, vf.createURI(propertyTwo), null, true);
        verify(connectorMock, never()).findStatements(eq(subjectUri), eq(vf.createURI(propertyOne)),
                any(org.openrdf.model.Value.class), anyBoolean());
        final Collection<Statement> allStatements = new ArrayList<>(stmts);
        allStatements.addAll(inferred);
        verifyReturnedAxioms(allStatements, res);
    }

	@Test
	public void testFindEntityReturnDataPropertyValueForObjectProperty() throws Exception {
		final AxiomDescriptor desc = new AxiomDescriptor(SUBJECT);
		final String propertyOne = "http://krizik.felk.cvut.cz/ontologies/jopa/properties#objectProperty";
		final List<Statement> statements = new ArrayList<>();
		final Assertion asOne = Assertion.createObjectPropertyAssertion(URI.create(propertyOne),
				false);
		desc.addAssertion(asOne);
		final String expected = "http://krizik.felk.cvut.cz/ontologies/jopa#entityOne";
		statements.add(vf.createStatement(subjectUri, vf.createURI(propertyOne),
				vf.createURI(expected)));
		// This statement should be filtered out by the adapter
		statements.add(vf.createStatement(subjectUri, vf.createURI(propertyOne),
				vf.createLiteral("someNonUriValue")));
		when(
				connectorMock.findStatements(subjectUri, vf.createURI(propertyOne), null, false)).thenReturn(statements);
		final Collection<Axiom<?>> res = adapter.find(desc);
		assertEquals(1, res.size());
		assertEquals(expected, res.iterator().next().getValue().toString());
	}

	@Test
	public void testFindEntityReturnObjectPropertyValueForDataProperty() throws Exception {
		final AxiomDescriptor desc = new AxiomDescriptor(SUBJECT);
		final String propertyOne = "http://krizik.felk.cvut.cz/ontologies/jopa/properties#dataProperty";
		final List<Statement> statements = new ArrayList<>();
		final Assertion asOne = Assertion.createDataPropertyAssertion(URI.create(propertyOne),
				false);
		desc.addAssertion(asOne);
		// This statement should be filtered out
		statements.add(vf.createStatement(subjectUri, vf.createURI(propertyOne),
				vf.createURI("http://krizik.felk.cvut.cz/ontologies/jopa#entityOne")));
		when(
				connectorMock.findStatements(subjectUri, vf.createURI(propertyOne), null, false,
						(org.openrdf.model.URI) null)).thenReturn(statements);
		final Collection<Axiom<?>> res = adapter.find(desc);
		assertTrue(res.isEmpty());
	}

	@Test
	public void testFindEntityWithBlankNodeInTypes() throws Exception {
		final AxiomDescriptor desc = new AxiomDescriptor(SUBJECT);
		final List<Statement> statements = new ArrayList<>();
		final Assertion clsAssertion = Assertion.createClassAssertion(false);
		desc.addAssertion(clsAssertion);
		statements.add(vf.createStatement(subjectUri, RDF.TYPE, vf.createBNode()));
		final String type = "http://krizik.felk.cvutcz/ontologies/jopa#entityA";
		statements.add(vf.createStatement(subjectUri, RDF.TYPE, vf.createURI(type)));
		when(connectorMock.findStatements(subjectUri, RDF.TYPE, null, false)).thenReturn(statements);

		final Collection<Axiom<?>> res = adapter.find(desc);
		assertEquals(1, res.size());
		final Axiom<?> ax = res.iterator().next();
		assertEquals(type, ax.getValue().stringValue());
	}

	@Test
	public void testGenerateIdentifier_ClassWithHash() throws Exception {
		final URI clsUri = URI.create("http://someClass.cz#class");
		when(
				connectorMock.findStatements(any(Resource.class), eq(RDF.TYPE),
						eq(vf.createURI(clsUri.toString())), eq(true))).thenReturn(
				Collections.<Statement> emptyList());
		final URI res = adapter.generateIdentifier(clsUri);
		assertNotNull(res);
		assertTrue(res.toString().contains(clsUri.toString()));
		verify(connectorMock).findStatements(vf.createURI(res.toString()), RDF.TYPE,
				vf.createURI(clsUri.toString()), true);
	}

	@Test
	public void testGenerateIdentifier_ClassWithoutHash() throws Exception {
		final URI clsUri = URI.create("http://someClass.cz/class");
		when(
				connectorMock.findStatements(any(Resource.class), eq(RDF.TYPE),
						eq(vf.createURI(clsUri.toString())), eq(true))).thenReturn(
				Collections.<Statement> emptyList());
		final URI res = adapter.generateIdentifier(clsUri);
		assertNotNull(res);
		assertTrue(res.toString().contains(clsUri.toString()));
		assertTrue(res.toString().contains("#"));
		verify(connectorMock).findStatements(vf.createURI(res.toString()), RDF.TYPE,
				vf.createURI(clsUri.toString()), true);
	}

	@Test
	public void testGenerateIdentifier_ClassEndsWithSlash() throws Exception {
		final URI clsUri = URI.create("http://someClass.cz/class/");
		when(
				connectorMock.findStatements(any(Resource.class), eq(RDF.TYPE),
						eq(vf.createURI(clsUri.toString())), eq(true))).thenReturn(
				Collections.<Statement> emptyList());
		final URI res = adapter.generateIdentifier(clsUri);
		assertNotNull(res);
		assertTrue(res.toString().contains(clsUri.toString()));
		verify(connectorMock).findStatements(vf.createURI(res.toString()), RDF.TYPE,
				vf.createURI(clsUri.toString()), true);
	}

	@Test(expected = IdentifierGenerationException.class)
	public void testGenerateIdentifierNeverUnique() throws Exception {
		final URI clsUri = URI.create("http://someClass.cz#class");
		final Collection<Statement> stmts = Collections.singletonList(mock(Statement.class));
		when(
				connectorMock.findStatements(any(Resource.class), eq(RDF.TYPE),
						eq(vf.createURI(clsUri.toString())), eq(true))).thenReturn(stmts);
		final URI res = adapter.generateIdentifier(clsUri);
		assert res == null;
	}

	@Test
	public void testRemove() throws Exception {
		final AxiomDescriptor desc = new AxiomDescriptor(SUBJECT);
		desc.addAssertion(Assertion.createClassAssertion(false));
		desc.addAssertion(Assertion.createDataPropertyAssertion(
				URI.create("http://krizik.felk.cvut.cz/dataProperty"), false));
		final Collection<Statement> statements = new HashSet<>(initStatementsForDescriptor(desc)
				.values());
		when(
				connectorMock.findStatements(eq(subjectUri), any(org.openrdf.model.URI.class),
						any(org.openrdf.model.Value.class), eq(false),
						any(org.openrdf.model.URI.class))).thenReturn(statements);

		adapter.remove(desc);
		for (Assertion ass : desc.getAssertions()) {
			verify(connectorMock).findStatements(subjectUri,
					vf.createURI(ass.getIdentifier().toString()), null, false,
					(org.openrdf.model.URI[]) null);
		}
		verify(connectorMock).removeStatements(statements);
	}

	@Test
	public void testContainsClassAssertion() throws Exception {
		final Axiom<URI> ax = new AxiomImpl<>(SUBJECT, Assertion.createClassAssertion(false),
				new Value<URI>(URI
						.create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")));
		final Set<Statement> result = new HashSet<>();
		result.add(mock(Statement.class));
		when(
				connectorMock.findStatements(eq(subjectUri), eq(RDF.TYPE),
						eq(vf.createURI(ax.getValue().stringValue())), anyBoolean(),
						eq((org.openrdf.model.URI[]) null))).thenReturn(result);

		assertTrue(adapter.contains(ax, null));
		verify(connectorMock).findStatements(subjectUri, RDF.TYPE,
				vf.createURI(ax.getValue().stringValue()), true, (org.openrdf.model.URI[]) null);
	}

	@Test
	public void testContainsClassAssertionInContext() throws Exception {
		final URI context = URI.create("http://context");
		final Axiom<URI> ax = new AxiomImpl<>(SUBJECT, Assertion.createClassAssertion(false),
				new Value<URI>(URI
						.create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")));
		final Set<Statement> result = new HashSet<>();
		result.add(mock(Statement.class));
		when(
				connectorMock.findStatements(eq(subjectUri), eq(RDF.TYPE),
						eq(vf.createURI(ax.getValue().stringValue())), anyBoolean(),
						any(org.openrdf.model.URI.class))).thenReturn(result);

		assertTrue(adapter.contains(ax, context));
		verify(connectorMock).findStatements(subjectUri, RDF.TYPE,
				vf.createURI(ax.getValue().stringValue()), true, vf.createURI(context.toString()));
	}

	@Test
	public void testContainsDataPropertyValue() throws Exception {
		final URI context = URI.create("http://context");
		final int val = 10;
		final Axiom<Integer> ax = new AxiomImpl<>(SUBJECT, Assertion.createClassAssertion(false),
				new Value<Integer>(val));
		final Set<Statement> result = new HashSet<>();
		when(
				connectorMock.findStatements(eq(subjectUri), eq(RDF.TYPE),
						eq(vf.createLiteral(val)), anyBoolean(), any(org.openrdf.model.URI.class)))
				.thenReturn(result);

		assertFalse(adapter.contains(ax, context));
		verify(connectorMock).findStatements(subjectUri, RDF.TYPE, vf.createLiteral(val), true,
				vf.createURI(context.toString()));
	}

	@Test
	public void updatesDataPropertyToNewValue() throws Exception {
		final AxiomValueDescriptor desc = new AxiomValueDescriptor(SUBJECT);
		final URI property = URI.create("http://krizik.felk.cvut.cz/dataProperty");
		final org.openrdf.model.URI sesameProperty = vf.createURI(property.toString());
		final boolean inferred = false;
		final Assertion assertion = Assertion.createDataPropertyAssertion(property, inferred);
		final String oldValue = "oldValue";
		final String newValue = "newValue";
		desc.addAssertion(assertion);
		desc.addAssertionValue(assertion, new Value<String>(newValue));
		final Collection<Statement> statements = Collections.singleton(vf.createStatement(
				subjectUri, sesameProperty, vf.createLiteral(oldValue, "en")));
		when(
				connectorMock.findStatements(eq(subjectUri), eq(sesameProperty),
						any(org.openrdf.model.Value.class), eq(inferred),
						any(org.openrdf.model.URI.class))).thenReturn(statements);

		adapter.update(desc);
		verify(connectorMock).findStatements(subjectUri, sesameProperty, null, inferred,
				(org.openrdf.model.URI) null);
		verify(connectorMock).removeStatements(statements);
		final Collection<Statement> inserted = Collections.singletonList(vf.createStatement(
				subjectUri, sesameProperty, vf.createLiteral(newValue, "en")));
		verify(connectorMock).addStatements(inserted);
	}

	@Test
	public void updatesObjectPropertyToNewValue() throws Exception {
		final AxiomValueDescriptor desc = new AxiomValueDescriptor(SUBJECT);
		final URI property = URI.create("http://krizik.felk.cvut.cz/objectProperty");
		final org.openrdf.model.URI sesameProperty = vf.createURI(property.toString());
		final boolean inferred = false;
		final Assertion assertion = Assertion.createObjectPropertyAssertion(property, inferred);
		final org.openrdf.model.URI oldValue = vf.createURI("http://www.old-value.org");
		final org.openrdf.model.URI newValue = vf.createURI("http://www.new-value.org");
		desc.addAssertion(assertion);
		desc.addAssertionValue(assertion, new Value<URI>(URI.create(newValue.stringValue())));
		final Collection<Statement> statements = Collections.singleton(vf.createStatement(
				subjectUri, sesameProperty, oldValue));
		when(
				connectorMock.findStatements(eq(subjectUri), eq(sesameProperty),
						any(org.openrdf.model.Value.class), eq(inferred),
						any(org.openrdf.model.URI.class))).thenReturn(statements);

		adapter.update(desc);
		verify(connectorMock).findStatements(subjectUri, sesameProperty, null, inferred,
				(org.openrdf.model.URI) null);
		verify(connectorMock).removeStatements(statements);
		final Collection<Statement> inserted = Collections.singletonList(vf.createStatement(
				subjectUri, sesameProperty, newValue));
		verify(connectorMock).addStatements(inserted);
	}

	@Test
	public void updatesObjectPropertyToEmptyValue() throws Exception {
		final AxiomValueDescriptor desc = new AxiomValueDescriptor(SUBJECT);
		final URI property = URI.create("http://krizik.felk.cvut.cz/objectProperty");
		final org.openrdf.model.URI sesameProperty = vf.createURI(property.toString());
		final boolean inferred = false;
		final Assertion assertion = Assertion.createObjectPropertyAssertion(property, inferred);
		final org.openrdf.model.URI oldValue = vf.createURI("http://www.old-value.org");
		desc.addAssertion(assertion);
		desc.addAssertionValue(assertion, Value.nullValue());
		final Collection<Statement> statements = Collections.singleton(vf.createStatement(
				subjectUri, sesameProperty, oldValue));
		when(
				connectorMock.findStatements(eq(subjectUri), eq(sesameProperty),
						any(org.openrdf.model.Value.class), eq(inferred),
						any(org.openrdf.model.URI.class))).thenReturn(statements);

		adapter.update(desc);
		verify(connectorMock).findStatements(subjectUri, sesameProperty, null, inferred,
				(org.openrdf.model.URI) null);
		verify(connectorMock).removeStatements(statements);
		verify(connectorMock, never()).addStatements(any(Collection.class));
	}

	@Test
	public void updatesTypesInContext() throws Exception {
		final AxiomValueDescriptor desc = new AxiomValueDescriptor(SUBJECT);
		final boolean inferred = false;
		final Assertion assertion = Assertion.createClassAssertion(inferred);
		final String[] newTypes = { "http://krizik.felk.cvut.cz/ontologies/types#tOne",
				"http://krizik.felk.cvut.cz/ontologies/types#tTwo",
				"http://krizik.felk.cvut.cz/ontologies/types#tFour",
				"http://krizik.felk.cvut.cz/ontologies/types#tFive" };
		desc.addAssertion(assertion);
		for (String t : newTypes) {
			desc.addAssertionValue(assertion, new Value<URI>(URI.create(t)));
		}
		final Collection<Statement> statements = initOldTypes();
		when(
				connectorMock.findStatements(eq(subjectUri), eq(RDF.TYPE),
						any(org.openrdf.model.Value.class), eq(inferred),
						any(org.openrdf.model.URI.class))).thenReturn(statements);

		adapter.update(desc);
		verify(connectorMock).findStatements(subjectUri, RDF.TYPE, null, inferred,
				(org.openrdf.model.URI) null);
		verify(connectorMock).removeStatements(statements);
		final Collection<Statement> inserted = initNewTypes(newTypes);
		verify(connectorMock).addStatements(inserted);
	}

	private Collection<Statement> initOldTypes() {
		final Collection<Statement> stmts = new HashSet<>();
		stmts.add(vf.createStatement(subjectUri, RDF.TYPE,
				vf.createURI("http://krizik.felk.cvut.cz/ontologies/types#tOne")));
		stmts.add(vf.createStatement(subjectUri, RDF.TYPE,
				vf.createURI("http://krizik.felk.cvut.cz/ontologies/types#tTwo")));
		stmts.add(vf.createStatement(subjectUri, RDF.TYPE,
				vf.createURI("http://krizik.felk.cvut.cz/ontologies/types#tThree")));
		return stmts;
	}

	private Collection<Statement> initNewTypes(String[] newTypes) {
		final Collection<Statement> stmts = new ArrayList<>();
		for (String t : newTypes) {
			stmts.add(vf.createStatement(subjectUri, RDF.TYPE, vf.createURI(t)));
		}
		return stmts;
	}
}

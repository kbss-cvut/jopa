/**
 * Copyright (C) 2022 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.IdentifierGenerationException;
import cz.cvut.kbss.ontodriver.model.*;
import cz.cvut.kbss.ontodriver.sesame.config.RuntimeConfiguration;
import cz.cvut.kbss.ontodriver.sesame.config.SesameConfigParam;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.environment.Generator;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class SesameAdapterTest {

    private static final NamedResource SUBJECT = NamedResource
            .create("http://krizik.felk.cvut.cz/ontologies/jopa/entityX");

    private static final ValueFactory VF = SimpleValueFactory.getInstance();
    private static org.eclipse.rdf4j.model.IRI subjectIri;

    @Mock
    private Connector connectorMock;

    private SesameAdapter adapter;

    @BeforeAll
    static void setUpBeforeClass() {
        subjectIri = VF.createIRI(SUBJECT.getIdentifier().toString());
    }

    @BeforeEach
    void setUp() {
        when(connectorMock.getValueFactory()).thenReturn(VF);
        final OntologyStorageProperties sp = OntologyStorageProperties.driver(SesameDataSource.class.getName())
                .physicalUri("memory-store").build();
        final DriverConfiguration configuration = new DriverConfiguration(sp);
        this.adapter = new SesameAdapter(connectorMock, configuration);

    }

    @Test
    void testSesameAdapter() throws Exception {
        final OntologyStorageProperties sp = OntologyStorageProperties.driver(SesameDataSource.class.getName())
                .physicalUri("memory-store").build();
        final DriverConfiguration dc = new DriverConfiguration(sp);
        dc.setProperty(SesameConfigParam.LOAD_ALL_THRESHOLD, "1");
        this.adapter = new SesameAdapter(connectorMock, dc);
        final Field configField = SesameAdapter.class.getDeclaredField("config");
        configField.setAccessible(true);
        final RuntimeConfiguration config = (RuntimeConfiguration) configField.get(adapter);
        assertEquals(1, config.getLoadAllThreshold());
    }

    @Test
    void testGetContexts() throws Exception {
        final List<Resource> contexts = new ArrayList<>();
        for (int i = 0; i < 5; i++) {
            contexts.add(VF.createIRI("http://krizik.felk.cvut.cz/ontologies/context" + i));
        }
        when(connectorMock.getContexts()).thenReturn(contexts);
        final List<URI> res = adapter.getContexts();
        assertEquals(contexts.size(), res.size());
        for (int i = 0; i < contexts.size(); i++) {
            assertEquals(contexts.get(i).stringValue(), res.get(i).toString());
        }
    }

    @Test
    void testGetContextsWithBNodes() throws Exception {
        final List<Resource> contexts = new ArrayList<>();
        int bnodes = 0;
        for (int i = 0; i < 5; i++) {
            if (i % 2 == 1) {
                contexts.add(VF.createBNode());
                bnodes++;
            } else {
                contexts.add(VF.createIRI("http://krizik.felk.cvut.cz/ontologies/context" + i));
            }
        }
        when(connectorMock.getContexts()).thenReturn(contexts);
        final List<URI> res = adapter.getContexts();
        assertEquals(contexts.size() - bnodes, res.size());
    }

    @Test
    void testClose() throws Exception {
        assertTrue(adapter.isOpen());
        adapter.close();
        verify(connectorMock).close();
        assertFalse(adapter.isOpen());
    }

    @Test
    void testPersistDataPropertiesNoContext() throws Exception {
        final AxiomValueDescriptor ad = new AxiomValueDescriptor(SUBJECT);
        ad.addAssertionValue(
                Assertion.createClassAssertion(false),
                new Value<>(URI
                        .create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")));
        ad.addAssertionValue(Assertion.createDataPropertyAssertion(URI
                        .create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute"),
                false), new Value<>("StringValue"));
        adapter.persist(ad);
        final ArgumentCaptor<Collection<Statement>> captor = ArgumentCaptor.forClass(Collection.class);
        verify(connectorMock).addStatements(captor.capture());
        final Collection<Statement> res = captor.getValue();
        assertEquals(2, res.size());
        assertTrue(statementsCorrespondToAxiomDescriptor(ad, res));
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
                        assertEquals(ad.getSubject().getIdentifier().toString(), stmt.getSubject().stringValue());
                        if (ad.getAssertionContext(as) == null) {
                            assertNull(stmt.getContext());
                        } else if (stmt.getContext() == null) {
                            assertNull(ad.getAssertionContext(as));
                        } else {
                            assertEquals(ad.getAssertionContext(as).toString(), stmt.getContext().stringValue());
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
    void testPersistEntityWithTypesDataPropertyInContext() throws Exception {
        final AxiomValueDescriptor ad = new AxiomValueDescriptor(SUBJECT);
        ad.addAssertionValue(
                Assertion.createClassAssertion(false),
                new Value<>(URI
                        .create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")));
        ad.addAssertionValue(
                Assertion.createClassAssertion(false),
                new Value<>(URI
                        .create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassC")));
        ad.addAssertionValue(
                Assertion.createClassAssertion(false),
                new Value<>(URI
                        .create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassD")));
        final Assertion dataAssertion = Assertion.createDataPropertyAssertion(URI
                        .create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute"),
                false);
        ad.addAssertionValue(dataAssertion, new Value<>("StringValue"));
        ad.setAssertionContext(dataAssertion,
                URI.create("http://krizik.felk.cvut.cz/ontologies/contextOne"));
        adapter.persist(ad);
        final ArgumentCaptor<Collection<Statement>> captor = ArgumentCaptor.forClass(Collection.class);
        verify(connectorMock).addStatements(captor.capture());
        final Collection<Statement> res = captor.getValue();
        assertEquals(4, res.size());
        assertTrue(statementsCorrespondToAxiomDescriptor(ad, res));
    }

    @Test
    void testPersistEntityWithObjectPropertyMultipleValues() throws Exception {
        final AxiomValueDescriptor ad = new AxiomValueDescriptor(SUBJECT);
        ad.addAssertionValue(
                Assertion.createClassAssertion(false),
                new Value<>(URI
                        .create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")));
        final Assertion objectAssertion = Assertion.createObjectPropertyAssertion(
                URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-owlclassY"),
                false);
        ad.addAssertionValue(objectAssertion,
                new Value<>(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityY")));
        ad.addAssertionValue(objectAssertion,
                new Value<>(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityYY")));
        ad.addAssertionValue(objectAssertion,
                new Value<>(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityYYY")));
        ad.setSubjectContext(URI.create("http://krizik.felk.cvut.cz/ontologies/contextOne"));
        adapter.persist(ad);
        final ArgumentCaptor<Collection<Statement>> captor = ArgumentCaptor.forClass(Collection.class);
        verify(connectorMock).addStatements(captor.capture());
        final Collection<Statement> res = captor.getValue();
        assertEquals(4, res.size());
        assertTrue(statementsCorrespondToAxiomDescriptor(ad, res));
    }

    @Test
    void testPersistEntityWithProperties() throws Exception {
        final AxiomValueDescriptor ad = new AxiomValueDescriptor(SUBJECT);
        ad.addAssertionValue(
                Assertion.createClassAssertion(false),
                new Value<>(URI
                        .create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")));
        final Assertion objectAssertion = Assertion.createPropertyAssertion(
                URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-owlclassY"),
                false);
        ad.addAssertionValue(objectAssertion,
                new Value<>(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityY")));
        final Assertion dataAssertion = Assertion.createPropertyAssertion(URI
                        .create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute"),
                false);
        ad.addAssertionValue(dataAssertion, new Value<>("stringValue"));
        adapter.persist(ad);
        final ArgumentCaptor<Collection<Statement>> captor = ArgumentCaptor.forClass(Collection.class);
        verify(connectorMock).addStatements(captor.capture());
        final Collection<Statement> res = captor.getValue();
        assertEquals(3, res.size());
        assertTrue(statementsCorrespondToAxiomDescriptor(ad, res));
    }

    @Test
    void testPersistEntityWithAnnotationPropertyAndDifferentSubjectAndPropertyContexts()
            throws Exception {
        final URI subjectCtx = URI.create("http://krizik.felk.cvut.cz/ontologies/contextOne");
        final URI propertyCtx = URI.create("http://krizik.felk.cvut.cz/ontologies/contextTwo");
        final AxiomValueDescriptor ad = new AxiomValueDescriptor(SUBJECT);
        ad.setSubjectContext(subjectCtx);
        ad.addAssertionValue(
                Assertion.createClassAssertion(false),
                new Value<>(URI
                        .create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")));
        final Assertion dataAssertion = Assertion.createAnnotationPropertyAssertion(
                URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-annotation"),
                false);
        ad.addAssertionValue(dataAssertion, new Value<>("StringValue"));
        ad.setAssertionContext(dataAssertion, propertyCtx);
        adapter.persist(ad);
        final ArgumentCaptor<Collection<Statement>> captor = ArgumentCaptor.forClass(Collection.class);
        verify(connectorMock).addStatements(captor.capture());
        final Collection<Statement> res = captor.getValue();
        assertEquals(2, res.size());
        assertTrue(statementsCorrespondToAxiomDescriptor(ad, res));
        // This checks that the contexts are set correctly
        for (Statement stmt : res) {
            if (stmt.getPredicate().stringValue().equals(dataAssertion.getIdentifier().toString())) {
                assertEquals(propertyCtx.toString(), stmt.getContext().stringValue());
            } else {
                assertEquals(subjectCtx.toString(), stmt.getContext().stringValue());
            }
        }
    }

    @Test
    void testFindEntityWithDataProperty() throws Exception {
        final AxiomDescriptor desc = new AxiomDescriptor(SUBJECT);
        desc.addAssertion(Assertion.createClassAssertion(false));
        final String property = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute";
        desc.addAssertion(Assertion.createDataPropertyAssertion(URI.create(property), false));
        final Map<Assertion, Statement> statements = initStatementsForDescriptor(desc);
        for (Assertion as : statements.keySet()) {
            final org.eclipse.rdf4j.model.IRI predicate = VF.createIRI(as.getIdentifier().toString());
            when(
                    connectorMock.findStatements(subjectIri, predicate, null, as.isInferred(), Collections.emptySet()))
                    .thenReturn(
                            Collections.singletonList(statements.get(as)));
        }
        final Collection<Axiom<?>> res = adapter.find(desc);
        verify(connectorMock, times(2)).findStatements(any(Resource.class),
                any(org.eclipse.rdf4j.model.IRI.class), any(), anyBoolean(),
                eq(Collections.emptySet()));
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
        final Resource subject = VF.createIRI(desc.getSubject().getIdentifier().toString());
        for (Assertion as : desc.getAssertions()) {
            final org.eclipse.rdf4j.model.IRI property = VF.createIRI(as.getIdentifier().toString());
            org.eclipse.rdf4j.model.Value val = null;
            switch (as.getType()) {
                case ANNOTATION_PROPERTY:
                    val = VF.createLiteral(
                            "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#annotationPropertyValue"
                                    + apCounter++);
                    break;
                case CLASS:
                    val = VF.createIRI("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA");
                    break;
                case DATA_PROPERTY:
                    val = VF.createLiteral("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#dataPropertyValue"
                            + dpCounter++);
                    break;
                case OBJECT_PROPERTY:
                    val = VF.createIRI("http://krizik.felk.cvut.cz/ontologies/jopa/entities#entityValue"
                            + opCounter++);
                    break;
                default:
                    break;
            }
            lst.put(as, VF.createStatement(subject, property, val));
        }
        return lst;
    }

    @Test
    void testFindEntityWithObjectPropertyAndInferredAnnotationProperty() throws Exception {
        final AxiomDescriptor desc = new AxiomDescriptor(SUBJECT);
        desc.addAssertion(Assertion.createClassAssertion(false));
        final String anProperty = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-annotationAttribute";
        desc.addAssertion(Assertion.createAnnotationPropertyAssertion(URI.create(anProperty), true));
        final String obProperty = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-refToA";
        desc.addAssertion(Assertion.createObjectPropertyAssertion(URI.create(obProperty), false));
        final Map<Assertion, Statement> statements = initStatementsForDescriptor(desc);
        for (Assertion as : statements.keySet()) {
            final org.eclipse.rdf4j.model.IRI predicate = VF.createIRI(as.getIdentifier().toString());
            when(
                    connectorMock.findStatements(subjectIri, predicate, null, as.isInferred(), Collections.emptySet()))
                    .thenReturn(
                            Collections.singletonList(statements.get(as)));
        }
        final Collection<Axiom<?>> res = adapter.find(desc);
        verify(connectorMock, times(3)).findStatements(any(Resource.class),
                any(org.eclipse.rdf4j.model.IRI.class), any(), anyBoolean(),
                eq(Collections.emptySet()));
        verify(connectorMock, times(1)).findStatements(eq(subjectIri),
                eq(VF.createIRI(anProperty)), eq(null), eq(true), eq(Collections.emptySet()));
        assertEquals(statements.size(), res.size());
        for (Axiom<?> ax : res) {
            assertTrue(statements.containsKey(ax.getAssertion()));
            assertEquals(statements.get(ax.getAssertion()).getObject().stringValue(), ax.getValue()
                    .getValue().toString());
        }
    }

    @Test
    void testFindEntityWithTypesAndSubjectContext() throws Exception {
        final AxiomDescriptor desc = new AxiomDescriptor(SUBJECT);
        final URI subjectCtx = URI.create("http://krizik.felk.cvut.cz/ontologies/contextOne");
        desc.addAssertion(Assertion.createClassAssertion(false));
        desc.addSubjectContext(subjectCtx);
        final List<Statement> statements = new ArrayList<>();
        final org.eclipse.rdf4j.model.IRI typeProperty = VF.createIRI(Assertion
                .createClassAssertion(false).getIdentifier().toString());
        final int count = 10;
        final Set<String> types = new HashSet<>();
        for (int i = 0; i < count; i++) {
            final org.eclipse.rdf4j.model.IRI type = VF
                    .createIRI("http://krizik.felk.cvut.cz/ontologies/jopa/types#Type" + i);
            statements.add(VF.createStatement(subjectIri, typeProperty, type));
            types.add(type.stringValue());
        }
        when(
                connectorMock.findStatements(subjectIri, typeProperty, null, false,
                        Collections.singleton(VF.createIRI(subjectCtx.toString())))).thenReturn(statements);
        final Collection<Axiom<?>> res = adapter.find(desc);
        assertEquals(statements.size(), res.size());
        for (Axiom<?> ax : res) {
            assertTrue(types.contains(ax.getValue().getValue().toString()));
        }
    }

    @Test
    void testFindEntityWithProperties() throws Exception {
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
                connectorMock.findStatements(subjectIri, null, null, false)).thenReturn(stmts);

        final Collection<Axiom<?>> res = adapter.find(desc);
        verify(connectorMock).findStatements(subjectIri, null, null, false);
        verify(connectorMock, never()).findStatements(eq(subjectIri), eq(VF.createIRI(propertyOne)),
                any(org.eclipse.rdf4j.model.Value.class), anyBoolean());
        verifyReturnedAxioms(stmts, res);
    }

    private List<Statement> createStatementsForProperty(String property, boolean objectProperty) {
        final List<Statement> res = new ArrayList<>();
        int cnt = 3;
        if (objectProperty) {
            for (int i = 0; i < cnt; i++) {
                res.add(VF.createStatement(subjectIri, VF.createIRI(property),
                        VF.createIRI("http://krizik.felk.cvut.cz/jopa#entity" + i)));
            }
        } else {
            for (int i = 0; i < cnt; i++) {
                res.add(VF.createStatement(subjectIri, VF.createIRI(property), VF.createLiteral(i)));
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
    void testFindEntityWithPropertiesAndInferredDataProperty() throws Exception {
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
        inferred.add(VF.createStatement(subjectIri, VF.createIRI(propertyTwo), VF.createLiteral(true)));
        statements.get(asTwo).addAll(inferred);
        when(connectorMock.findStatements(subjectIri, null, null, false)).thenReturn(stmts);
        when(connectorMock.findStatements(subjectIri, VF.createIRI(propertyTwo), null, true, Collections.emptySet()))
                .thenReturn(statements.get(asTwo));

        final Collection<Axiom<?>> res = adapter.find(desc);
        verify(connectorMock).findStatements(subjectIri, VF.createIRI(propertyTwo), null, true, Collections.emptySet());
        verify(connectorMock).findStatements(subjectIri, null, null, false);
        verify(connectorMock, never()).findStatements(eq(subjectIri), eq(VF.createIRI(propertyOne)),
                any(org.eclipse.rdf4j.model.Value.class), anyBoolean(), eq(null));
        final Collection<Statement> allStatements = new ArrayList<>(stmts);
        allStatements.addAll(inferred);
        verifyReturnedAxioms(allStatements, res);
    }

    @Test
    void testFindEntityReturnDataPropertyValueForObjectProperty() throws Exception {
        final AxiomDescriptor desc = new AxiomDescriptor(SUBJECT);
        final String propertyOne = "http://krizik.felk.cvut.cz/ontologies/jopa/properties#objectProperty";
        final List<Statement> statements = new ArrayList<>();
        final Assertion asOne = Assertion.createObjectPropertyAssertion(URI.create(propertyOne),
                false);
        desc.addAssertion(asOne);
        final String expected = "http://krizik.felk.cvut.cz/ontologies/jopa#entityOne";
        statements.add(VF.createStatement(subjectIri, VF.createIRI(propertyOne),
                VF.createIRI(expected)));
        // This statement should be filtered out by the adapter
        statements.add(VF.createStatement(subjectIri, VF.createIRI(propertyOne),
                VF.createLiteral("someNonUriValue")));
        when(
                connectorMock
                        .findStatements(subjectIri, VF.createIRI(propertyOne), null, false, Collections.emptySet()))
                .thenReturn(statements);
        final Collection<Axiom<?>> res = adapter.find(desc);
        assertEquals(1, res.size());
        assertEquals(expected, res.iterator().next().getValue().toString());
    }

    @Test
    void testFindEntityReturnObjectPropertyValueForDataProperty() throws Exception {
        final AxiomDescriptor desc = new AxiomDescriptor(SUBJECT);
        final String propertyOne = "http://krizik.felk.cvut.cz/ontologies/jopa/properties#dataProperty";
        final List<Statement> statements = new ArrayList<>();
        final Assertion asOne = Assertion.createDataPropertyAssertion(URI.create(propertyOne),
                false);
        desc.addAssertion(asOne);
        // This statement should be filtered out
        statements.add(VF.createStatement(subjectIri, VF.createIRI(propertyOne),
                VF.createIRI("http://krizik.felk.cvut.cz/ontologies/jopa#entityOne")));
        when(
                connectorMock.findStatements(eq(subjectIri), eq(VF.createIRI(propertyOne)), any(), eq(false),
                        anyCollection())).thenReturn(statements);
        final Collection<Axiom<?>> res = adapter.find(desc);
        assertTrue(res.isEmpty());
    }

    @Test
    void findWithObjectPropertyAndPropertiesReturnsAxiomForObjectProperty() throws Exception {
        final AxiomDescriptor desc = new AxiomDescriptor(SUBJECT);
        final String propertyOne = "http://krizik.felk.cvut.cz/ontologies/jopa/properties#objectProperty";
        final List<Statement> statements = new ArrayList<>();
        statements.add(VF.createStatement(subjectIri, VF.createIRI(propertyOne),
                VF.createIRI("http://krizik.felk.cvut.cz/ontologies/jopa#entityOne")));
        final Assertion asOne = Assertion.createObjectPropertyAssertion(URI.create(propertyOne),
                false);
        final Set<Assertion> assertions = new LinkedHashSet<>();
        assertions.add(Assertion.createUnspecifiedPropertyAssertion(false));
        assertions.add(asOne);
        assertions.forEach(desc::addAssertion);
        when(connectorMock.findStatements(subjectIri, null, null, false)).thenReturn(statements);
        final Collection<Axiom<?>> axioms = adapter.find(desc);
        assertEquals(1, axioms.size());
        final Assertion a = axioms.iterator().next().getAssertion();
        assertEquals(asOne, a);
    }

    @Test
    void testFindEntityWithBlankNodeInTypes() throws Exception {
        final AxiomDescriptor desc = new AxiomDescriptor(SUBJECT);
        final List<Statement> statements = new ArrayList<>();
        final Assertion clsAssertion = Assertion.createClassAssertion(false);
        desc.addAssertion(clsAssertion);
        statements.add(VF.createStatement(subjectIri, RDF.TYPE, VF.createBNode()));
        final String type = "http://krizik.felk.cvutcz/ontologies/jopa#entityA";
        statements.add(VF.createStatement(subjectIri, RDF.TYPE, VF.createIRI(type)));
        when(connectorMock.findStatements(subjectIri, RDF.TYPE, null, false, Collections.emptySet()))
                .thenReturn(statements);

        final Collection<Axiom<?>> res = adapter.find(desc);
        assertEquals(1, res.size());
        final Axiom<?> ax = res.iterator().next();
        assertEquals(type, ax.getValue().stringValue());
    }

    @Test
    void testGenerateIdentifier_ClassWithHash() throws Exception {
        final URI clsUri = URI.create("http://someClass.cz#class");
        when(
                connectorMock.containsStatement(any(Resource.class), eq(RDF.TYPE),
                        eq(VF.createIRI(clsUri.toString())), eq(true), eq(Collections.emptySet()))).thenReturn(false);
        final URI res = adapter.generateIdentifier(clsUri);
        assertNotNull(res);
        assertTrue(res.toString().contains(clsUri.toString()));
        verify(connectorMock).containsStatement(VF.createIRI(res.toString()), RDF.TYPE,
                VF.createIRI(clsUri.toString()), true, Collections.emptySet());
    }

    @Test
    void testGenerateIdentifier_ClassWithoutHash() throws Exception {
        final URI clsUri = URI.create("http://someClass.cz/class");
        when(
                connectorMock.containsStatement(any(Resource.class), eq(RDF.TYPE),
                        eq(VF.createIRI(clsUri.toString())), eq(true), eq(Collections.emptySet()))).thenReturn(false);
        final URI res = adapter.generateIdentifier(clsUri);
        assertNotNull(res);
        assertTrue(res.toString().contains(clsUri.toString()));
        assertTrue(res.toString().contains("/instance"));
        verify(connectorMock).containsStatement(VF.createIRI(res.toString()), RDF.TYPE,
                VF.createIRI(clsUri.toString()), true, Collections.emptySet());
    }

    @Test
    void testGenerateIdentifier_ClassEndsWithSlash() throws Exception {
        final URI clsUri = URI.create("http://someClass.cz/class/");
        when(
                connectorMock.containsStatement(any(Resource.class), eq(RDF.TYPE),
                        eq(VF.createIRI(clsUri.toString())), eq(true), eq(Collections.emptySet()))).thenReturn(false);
        final URI res = adapter.generateIdentifier(clsUri);
        assertNotNull(res);
        assertTrue(res.toString().contains(clsUri.toString()));
        verify(connectorMock).containsStatement(VF.createIRI(res.toString()), RDF.TYPE,
                VF.createIRI(clsUri.toString()), true, Collections.emptySet());
    }

    @Test
    void testGenerateIdentifierNeverUnique() throws Exception {
        final URI clsUri = URI.create("http://someClass.cz#class");
        when(
                connectorMock.containsStatement(any(Resource.class), eq(RDF.TYPE),
                        eq(VF.createIRI(clsUri.toString())), eq(true), eq(Collections.emptySet()))).thenReturn(true);
        assertThrows(IdentifierGenerationException.class, () -> adapter.generateIdentifier(clsUri));
    }

    @Test
    void testRemove() throws Exception {
        final AxiomDescriptor desc = new AxiomDescriptor(SUBJECT);
        desc.addAssertion(Assertion.createClassAssertion(false));
        desc.addAssertion(Assertion.createDataPropertyAssertion(
                URI.create("http://krizik.felk.cvut.cz/dataProperty"), false));
        final Collection<Statement> statements = new HashSet<>(initStatementsForDescriptor(desc)
                .values());
        when(connectorMock
                .findStatements(eq(subjectIri), any(org.eclipse.rdf4j.model.IRI.class), any(), eq(false),
                        eq(Collections.emptySet())))
                .thenReturn(statements);

        adapter.remove(desc);
        for (Assertion ass : desc.getAssertions()) {
            verify(connectorMock).findStatements(subjectIri,
                    VF.createIRI(ass.getIdentifier().toString()), null, false, Collections.emptySet());
        }
        verify(connectorMock).removeStatements(statements);
    }

    @Test
    void testContainsClassAssertion() throws Exception {
        final Axiom<URI> ax = new AxiomImpl<>(SUBJECT, Assertion.createClassAssertion(false),
                new Value<>(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")));
        when(connectorMock
                .containsStatement(eq(subjectIri), eq(RDF.TYPE), eq(VF.createIRI(ax.getValue().stringValue())),
                        anyBoolean(), anySet())).thenReturn(true);

        assertTrue(adapter.contains(ax, Collections.emptySet()));
        verify(connectorMock).containsStatement(subjectIri, RDF.TYPE,
                VF.createIRI(ax.getValue().stringValue()), ax.getAssertion().isInferred(), Collections.emptySet());
    }

    @Test
    void testContainsClassAssertionInContext() throws Exception {
        final URI context = URI.create("http://context");
        final Axiom<URI> ax = new AxiomImpl<>(SUBJECT, Assertion.createClassAssertion(false),
                new Value<>(URI
                        .create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")));
        when(
                connectorMock.containsStatement(eq(subjectIri), eq(RDF.TYPE),
                        eq(VF.createIRI(ax.getValue().stringValue())), anyBoolean(),
                        anyCollection())).thenReturn(true);

        assertTrue(adapter.contains(ax, Collections.singleton(context)));
        verify(connectorMock).containsStatement(subjectIri, RDF.TYPE,
                VF.createIRI(ax.getValue().stringValue()), ax.getAssertion().isInferred(),
                Collections.singleton(VF.createIRI(context.toString())));
    }

    @Test
    void testContainsDataPropertyValue() throws Exception {
        final URI context = URI.create("http://context");
        final int val = 10;
        final Axiom<Integer> ax = new AxiomImpl<>(SUBJECT, Assertion.createClassAssertion(false),
                new Value<>(val));
        when(
                connectorMock.containsStatement(eq(subjectIri), eq(RDF.TYPE),
                        eq(VF.createLiteral(val)), anyBoolean(), anyCollection()))
                .thenReturn(false);

        assertFalse(adapter.contains(ax, Collections.singleton(context)));
        verify(connectorMock)
                .containsStatement(subjectIri, RDF.TYPE, VF.createLiteral(val), ax.getAssertion().isInferred(),
                        Collections.singleton(VF.createIRI(context.toString())));
    }

    @Test
    void updatesDataPropertyToNewValue() throws Exception {
        final AxiomValueDescriptor desc = new AxiomValueDescriptor(SUBJECT);
        final URI property = URI.create("http://krizik.felk.cvut.cz/dataProperty");
        final org.eclipse.rdf4j.model.IRI sesameProperty = VF.createIRI(property.toString());
        final boolean inferred = false;
        final Assertion assertion = Assertion.createDataPropertyAssertion(property, "en", inferred);
        final String oldValue = "oldValue";
        final String newValue = "newValue";
        desc.addAssertion(assertion);
        desc.addAssertionValue(assertion, new Value<>(newValue));
        final Collection<Statement> statements = Collections.singleton(VF.createStatement(
                subjectIri, sesameProperty, VF.createLiteral(oldValue, "en")));
        when(connectorMock
                .findStatements(eq(subjectIri), eq(sesameProperty), any(), eq(inferred), eq(Collections.emptySet())))
                .thenReturn(statements);

        adapter.update(desc);
        verify(connectorMock).findStatements(subjectIri, sesameProperty, null, inferred, Collections.emptySet());
        verify(connectorMock).removeStatements(statements);
        final Collection<Statement> inserted = Collections.singletonList(VF.createStatement(
                subjectIri, sesameProperty, VF.createLiteral(newValue, "en")));
        verify(connectorMock).addStatements(inserted);
    }

    @Test
    void updatesObjectPropertyToNewValue() throws Exception {
        final AxiomValueDescriptor desc = new AxiomValueDescriptor(SUBJECT);
        final URI property = URI.create("http://krizik.felk.cvut.cz/objectProperty");
        final org.eclipse.rdf4j.model.IRI sesameProperty = VF.createIRI(property.toString());
        final boolean inferred = false;
        final Assertion assertion = Assertion.createObjectPropertyAssertion(property, inferred);
        final org.eclipse.rdf4j.model.IRI oldValue = VF.createIRI("http://www.old-value.org");
        final org.eclipse.rdf4j.model.IRI newValue = VF.createIRI("http://www.new-value.org");
        desc.addAssertion(assertion);
        desc.addAssertionValue(assertion, new Value<>(URI.create(newValue.stringValue())));
        final Collection<Statement> statements = Collections.singleton(VF.createStatement(
                subjectIri, sesameProperty, oldValue));
        when(connectorMock
                .findStatements(eq(subjectIri), eq(sesameProperty), any(), eq(inferred), eq(Collections.emptySet())))
                .thenReturn(statements);

        adapter.update(desc);
        verify(connectorMock).findStatements(subjectIri, sesameProperty, null, inferred, Collections.emptySet());
        verify(connectorMock).removeStatements(statements);
        final Collection<Statement> inserted = Collections.singletonList(VF.createStatement(
                subjectIri, sesameProperty, newValue));
        verify(connectorMock).addStatements(inserted);
    }

    @Test
    void updatesObjectPropertyToEmptyValue() throws Exception {
        final AxiomValueDescriptor desc = new AxiomValueDescriptor(SUBJECT);
        final URI property = URI.create("http://krizik.felk.cvut.cz/objectProperty");
        final org.eclipse.rdf4j.model.IRI sesameProperty = VF.createIRI(property.toString());
        final boolean inferred = false;
        final Assertion assertion = Assertion.createObjectPropertyAssertion(property, inferred);
        final org.eclipse.rdf4j.model.IRI oldValue = VF.createIRI("http://www.old-value.org");
        desc.addAssertion(assertion);
        desc.addAssertionValue(assertion, Value.nullValue());
        final Collection<Statement> statements = Collections.singleton(VF.createStatement(
                subjectIri, sesameProperty, oldValue));
        when(connectorMock
                .findStatements(eq(subjectIri), eq(sesameProperty), any(), eq(inferred), eq(Collections.emptySet())))
                .thenReturn(statements);

        adapter.update(desc);
        verify(connectorMock).findStatements(subjectIri, sesameProperty, null, inferred, Collections.emptySet());
        verify(connectorMock).removeStatements(statements);
        verify(connectorMock, never()).addStatements(anyCollection());
    }

    @Test
    void updatesTypesInContext() throws Exception {
        final AxiomValueDescriptor desc = new AxiomValueDescriptor(SUBJECT);
        final boolean inferred = false;
        final Assertion assertion = Assertion.createClassAssertion(inferred);
        final String[] newTypes = {"http://krizik.felk.cvut.cz/ontologies/types#tOne",
                "http://krizik.felk.cvut.cz/ontologies/types#tTwo",
                "http://krizik.felk.cvut.cz/ontologies/types#tFour",
                "http://krizik.felk.cvut.cz/ontologies/types#tFive"};
        desc.addAssertion(assertion);
        final URI context = Generator.generateUri();
        final IRI iriContext = VF.createIRI(context.toString());
        desc.setAssertionContext(assertion, context);
        for (String t : newTypes) {
            desc.addAssertionValue(assertion, new Value<>(URI.create(t)));
        }
        final Collection<Statement> statements = initOldTypes();
        when(connectorMock.findStatements(eq(subjectIri), eq(RDF.TYPE), any(), eq(inferred),
                eq(Collections.singleton(iriContext))))
                .thenReturn(statements);

        adapter.update(desc);
        verify(connectorMock).findStatements(subjectIri, RDF.TYPE, null, inferred, Collections.singleton(iriContext));
        verify(connectorMock).removeStatements(statements);
        final Collection<Statement> inserted = initNewTypes(newTypes, iriContext);
        verify(connectorMock).addStatements(inserted);
    }

    private Collection<Statement> initOldTypes() {
        final Collection<Statement> stmts = new HashSet<>();
        stmts.add(VF.createStatement(subjectIri, RDF.TYPE,
                VF.createIRI("http://krizik.felk.cvut.cz/ontologies/types#tOne")));
        stmts.add(VF.createStatement(subjectIri, RDF.TYPE,
                VF.createIRI("http://krizik.felk.cvut.cz/ontologies/types#tTwo")));
        stmts.add(VF.createStatement(subjectIri, RDF.TYPE,
                VF.createIRI("http://krizik.felk.cvut.cz/ontologies/types#tThree")));
        return stmts;
    }

    private Collection<Statement> initNewTypes(String[] newTypes, IRI context) {
        final Collection<Statement> statements = new ArrayList<>();
        for (String t : newTypes) {
            statements.add(VF.createStatement(subjectIri, RDF.TYPE, VF.createIRI(t), context));
        }
        return statements;
    }

    @Test
    void unwrapReturnsItselfWhenClassMatches() throws Exception {
        assertSame(adapter, adapter.unwrap(SesameAdapter.class));
    }

    @Test
    void unwrapReturnValueFactoryWhenItMatches() throws Exception {
        assertSame(VF, adapter.unwrap(ValueFactory.class));
    }

    @Test
    void getTypesHandlerStartsTransaction() throws SesameDriverException {
        adapter.getTypesHandler();
        verify(connectorMock).begin();
    }

    @Test
    void getSimpleListHandlerStartsTransaction() throws SesameDriverException {
        adapter.getSimpleListHandler();
        verify(connectorMock).begin();
    }

    @Test
    void getReferencedListHandlerStartsTransaction() throws Exception {
        adapter.getReferencedListHandler();
        verify(connectorMock).begin();
    }
}
